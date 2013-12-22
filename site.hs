--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.Monoid (mappend)
import Data.List (isInfixOf)
import System.FilePath.Posix  (takeBaseName,takeDirectory,(</>),splitFileName)
import Text.Pandoc

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match (fromList prebuiltFiles) $ do
    route idRoute
    compile copyFileCompiler
  
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/main.scss" $ do
    route $ setExtension "css"
    compile $ getResourceString >>= sassify

  match (fromList pagesWithToc) $ do
    route niceRoute
    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocTocWriter
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls
      >>= removeIndexHtml

  match "posts/*" $ do
    route niceDateRoute
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls
      >>= removeIndexHtml

  create ["archive.html"] $ do
    route niceRoute
    compile $ do
      let archiveCtx =
            field "posts" (\_ -> postList recentFirst) `mappend`
            constField "title" "Archive"               `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
        >>= removeIndexHtml

  match "index.html" $ do
    route idRoute
    compile $ do
      let indexCtx = field "posts" $ \_ ->
            completePostList $ fmap (take 5) . recentFirst

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
        >>= removeIndexHtml

  match "templates/*" $ compile templateCompiler

  where pagesWithToc = ["about.markdown", "cocoa-coding-conventions.markdown"]
        pandocTocWriter = defaultHakyllWriterOptions { writerTableOfContents = True
                                                     , writerTemplate = "$toc$\n$body$"
                                                     , writerStandalone = True }
        prebuiltFiles = ["CNAME", "humans.txt", "robots.txt"]
      

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts   <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl postCtx posts
  
--------------------------------------------------------------------------------
-- | Returns a list of post bodies
completePostList :: ([Item String] -> Compiler [Item String]) -> Compiler String
completePostList sortFilter = do
  posts   <- sortFilter =<< loadAllSnapshots "posts/*" "content"
  itemTpl <- loadBody "templates/post-with-link.html"
  applyTemplateList itemTpl postCtx posts
  
--------------------------------------------------------------------------------
dateRoute :: Routes
dateRoute = gsubRoute "posts/" (const "") `composeRoutes`
            gsubRoute "[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceChars)
  where
    replaceChars c | c == '-' || c == '_' = '/'
                   | otherwise = c

--------------------------------------------------------------------------------
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      takeDirectory p </> takeBaseName p </> "index.html"
      where
        p = toFilePath ident
        
--------------------------------------------------------------------------------
-- |Turns 2012-02-01-post.html into 2012/02/01/post/index.html
niceDateRoute :: Routes
niceDateRoute = composeRoutes dateRoute niceRoute

--------------------------------------------------------------------------------
-- |Replace an url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

--------------------------------------------------------------------------------
-- |Removes the .html component of a URL if it is local
removeIndexStr :: String -> String
removeIndexStr url = case splitFileName url of
  (dir, "index.html") | isLocal dir -> dir
                      | otherwise   -> url
  _                                 -> url
  where
    isLocal uri = not ("://" `isInfixOf` uri)
      
--------------------------------------------------------------------------------
-- |Run sass and compress the result
sassify :: Item String -> Compiler (Item String)
sassify item = withItemBody (unixFilter "sass" ["-s", "--scss", "--load-path", "css"]) item
--               >>= return . fmap compressCss
