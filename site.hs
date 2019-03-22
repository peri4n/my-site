{-# LANGUAGE OverloadedStrings #-}

import           Data.FileStore               (Revision, authorName, revAuthor,
                                               revDateTime, revId)
import           Data.Monoid
import           Hakyll
import           Hakyll.FileStore.Git.Context (gitGetAuthorNames,
                                               gitGetRevisions)
import           Hakyll.Web.CompressCss       (compressCss)
import           Hakyll.Web.Template.Context  (metadataField)
import           System.Environment           (getArgs)
import           System.Process               (readProcess)

main :: IO ()
main = do
  draftMode <- fmap (elem "--with-drafts") getArgs
  let postsPattern =
        if draftMode
          then "posts/*" .||. "drafts/*"
          else "posts/*"

  hakyllWith myConfig $ do
    match "fonts/*" $ do
      route idRoute
      compile copyFileCompiler

    match "sass/**.scss" $ compile getResourceBody
    scssDependencies <- makePatternDependency "sass/**.scss"
    rulesExtraDependencies [scssDependencies] $
      create ["css/main.css"] $ do
        route idRoute
        compile compressedSassCompiler

    match postsPattern $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx >>=
        loadAndApplyTemplate "templates/default.html" postCtx >>=
        relativizeUrls

    match (fromList ["projects.md", "contact.md", "about.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>= loadAndApplyTemplate "templates/default.html" pageCtx >>=
        relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Home" `mappend`
              pageCtx
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------
myDefaultCtx :: Context String
myDefaultCtx = defaultContext <> dateCtx <> metadataField

dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y"

postCtx :: Context String
postCtx = myDefaultCtx <> gitCtx

pageCtx :: Context String
pageCtx = myDefaultCtx

metaKeywordCtx :: Context String
metaKeywordCtx =
  field "metaKeywords" $ \item -> do
    tags <- getMetadataField (itemIdentifier item) "keywords"
    return $ maybe "" showMetaTags tags
  where
    showMetaTags t = "<meta name=\"keywords\" content=\"" ++ t ++ "\">\n"

gitCtx :: Context String
gitCtx = gitAuthorCtx <> gitRevisionCtx <> gitDateCtx

fromLastRevisionCtx :: String -> (Revision -> String) -> Context String
fromLastRevisionCtx name f =
  field name (\item -> f . head <$> gitGetRevisions (itemIdentifier item))

gitAuthorCtx :: Context String
gitAuthorCtx = fromLastRevisionCtx "author" (authorName . revAuthor)

gitRevisionCtx :: Context String
gitRevisionCtx = fromLastRevisionCtx "revision" revId

gitDateCtx :: Context String
gitDateCtx = fromLastRevisionCtx "lastmod" (show . revDateTime)

--------------------------------------------------------------------------------
-- Compilers
--------------------------------------------------------------------------------
sassCompiler :: Compiler (Item String)
sassCompiler =
  loadBody (fromFilePath "sass/main.scss") >>= makeItem >>=
  withItemBody (unixFilter "sass" args)
  where
    args = ["-s", "--scss", "-I", "sass"]

compressedSassCompiler :: Compiler (Item String)
compressedSassCompiler = fmap compressCss <$> sassCompiler

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------
myConfig :: Configuration
myConfig =
  defaultConfiguration
    {deployCommand = "aws s3 cp ./_site/ s3://bioinform.at/ --recursive"}
