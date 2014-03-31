{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

main :: IO ()
main = hakyll $ do

    match "pages/index.html" $ do
        route   pagesRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/index.html" defaultContext
            >>= relativizeUrls

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" (archiveCtx posts)
                >>= loadAndApplyTemplate "templates/default.html" (archiveCtx posts)
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

archiveCtx :: [Item String] -> Context String
archiveCtx posts =
    listField "posts" postCtx (return posts) `mappend`
    constField "title" "Posts"               `mappend`
    defaultContext

pagesRoute :: Routes
pagesRoute = gsubRoute "pages/" $ const ""
