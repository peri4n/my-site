{-# LANGUAGE OverloadedStrings #-}

import Data.FileStore
  ( Revision,
    authorName,
    revAuthor,
    revDateTime,
    revId,
  )
import Data.Monoid ((<>))
import Hakyll
import Hakyll.FileStore.Git.Context
  ( gitGetAuthorNames,
    gitGetRevisions,
  )
import Hakyll.Web.CompressCss (compressCss)
import Hakyll.Web.Template.Context (metadataField)

main :: IO ()
main = do
  let postsPattern = "posts/*"

  hakyllWith myConfig $ do
    -- Process fonts
    match "fonts/*" $ do
      route idRoute
      compile copyFileCompiler

    -- Process styles
    match "sass/**.scss" $
      compile getResourceBody
    scssDependencies <- makePatternDependency "sass/**.scss"
    rulesExtraDependencies [scssDependencies] $
      create ["css/main.css"] $ do
        route idRoute
        compile compressedSassCompiler

    -- Process tags
    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    -- Process articles
    match (postsPattern .||. fromList ["projects.md", "contact.md", "about.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
          >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
          >>= relativizeUrls

    -- Process tags pages
    tagsRules tags $ \tag pat -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pat
        let ctx =
              constField "title" title
                `mappend` listField "posts" (postCtxWithTags tags) (return posts)
                `mappend` defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tags.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    -- Process index page
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts)
                `mappend` constField "title" "Home"
                `mappend` defaultContext
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------
myDefaultCtx :: Context String
myDefaultCtx = defaultContext <> dateCtx <> metadataField

dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y"

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> defaultContext <> gitCtx <> dateCtx

postCtx :: Context String
postCtx = myDefaultCtx <> gitCtx

pageCtx :: Context String
pageCtx = myDefaultCtx <> gitCtx

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
  loadBody (fromFilePath "sass/main.scss") >>= makeItem
    >>= withItemBody (unixFilter "sass" args)
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
    { deployCommand = "aws s3 cp ./_site/ s3://fbull.de/ --recursive --profile private"
    }
