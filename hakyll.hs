{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
--import           Data.Maybe  (fromMaybe)
--import qualified Data.Map as M
import           Hakyll

main :: IO ()
main = hakyll $ do

    match "pages/index.html" $ do
        route   pagesRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/index.html" defaultContext
            >>= relativizeUrls

    match "pages/*" $ do
        route   pagesRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "drafts/*" postRules
    match "posts/*"  postRules

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" (postsCtx posts)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "stuff/*" $ do
        route   pagesRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["stuff.html"] $ do
        route idRoute
        compile $ do
            stuff <- loadAll "stuff/*"
            makeItem ""
                >>= loadAndApplyTemplate "templates/stuff-list.html" (stuffCtx stuff)
                >>= loadAndApplyTemplate "templates/default.html"    defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

postRules :: Rules ()
postRules = do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postsCtx :: [Item String] -> Context String
postsCtx posts =
    listField "posts" postCtx (return posts) `mappend`
    constField "title" "Posts"               `mappend`
    defaultContext

--titleContext :: Context a
--titleContext = field "title" $ \item -> do
--    metadata <- getMetadata (itemIdentifier item)
--    return $ fromMaybe "No title" $ M.lookup "title" metadata

stuffCtx :: [Item String] -> Context String
stuffCtx stuff =
    listField "stuff" defaultContext (return stuff) `mappend`
    --titleContext                                    `mappend`
    constField "title" "Stuff"                      `mappend`
    defaultContext

pagesRoute :: Routes
pagesRoute = gsubRoute "pages/" $ const ""
