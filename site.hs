--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith myConfig $ do

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "sass/**.scss" $ do
      compile getResourceBody

    scssDependencies <- makePatternDependency "sass/**.scss"
    rulesExtraDependencies [scssDependencies] $ do
      create ["css/main.css"] $ do
        route   idRoute
        compile sassCompiler

    match ("posts/*" .||. fromList ["projects.md", "contact.md", "about.md"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

sassCompiler :: Compiler (Item String)
sassCompiler = loadBody (fromFilePath "sass/main.scss")
                 >>= makeItem
                 >>= withItemBody (unixFilter "sass" args)
  where args = ["-s", "--scss", "-I", "sass"]

--------------------------------------------------------------------------------
myConfig :: Configuration
myConfig = defaultConfiguration {
    deployCommand = "aws s3 cp ./_site/ s3://bioinform.at/ --recursive"
               }
