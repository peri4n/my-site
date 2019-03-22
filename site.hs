--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import           Hakyll.Web.CompressCss (compressCss)
import           System.Environment (getArgs)
import           System.Process (readProcess)

--------------------------------------------------------------------------------
main :: IO ()
main = do
    draftMode <- fmap (elem "--with-drafts") getArgs
    let postsPattern = if draftMode then "posts/*" .||. "drafts/*" else "posts/*"

    hakyllWith myConfig $ do

        match "fonts/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "sass/**.scss" $ do
          compile getResourceBody

        scssDependencies <- makePatternDependency "sass/**.scss"
        rulesExtraDependencies [scssDependencies] $ do
          create ["css/main.css"] $ do
            route   idRoute
            compile compressedSassCompiler

        match postsPattern $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        match (fromList ["projects.md", "contact.md", "about.md"]) $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    pageCtx
                >>= loadAndApplyTemplate "templates/default.html" pageCtx
                >>= relativizeUrls

        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Home"                `mappend`
                        pageCtx

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <> metaKeywordCtx <> gitCtx <> defaultContext

pageCtx :: Context String
pageCtx =
    dateField "date" "%B %e, %Y" <> defaultContext <> gitCtx

metaKeywordCtx :: Context String
metaKeywordCtx = field "metaKeywords" $ \item -> do
    tags <- getMetadataField (itemIdentifier item) "keywords"
    return $ maybe "" showMetaTags tags
        where
            showMetaTags t = "<meta name=\"keywords\" content=\"" ++ t ++ "\">\n"

gitCtx :: Context String
gitCtx = field "gitTag" $ \item ->
    unsafeCompiler $ do
        let fileName = itemIdentifier item
        sha <- readProcess "git" ["hash-object", toFilePath fileName] ""
        return sha

--------------------------------------------------------------------------------
-- Compilers
--------------------------------------------------------------------------------
sassCompiler :: Compiler (Item String)
sassCompiler = loadBody (fromFilePath "sass/main.scss")
                 >>= makeItem
                 >>= withItemBody (unixFilter "sass" args)
    where args = ["-s", "--scss", "-I", "sass"]

compressedSassCompiler :: Compiler (Item String)
compressedSassCompiler = fmap compressCss <$> sassCompiler

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------
myConfig :: Configuration
myConfig = defaultConfiguration {
    deployCommand = "aws s3 cp ./_site/ s3://bioinform.at/ --recursive"
               }
