--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.Monoid ((<>), mconcat, mappend)
import Data.List (isInfixOf)
import System.FilePath.Posix  (takeBaseName,takeDirectory,(</>),splitFileName)
import Text.Pandoc

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*" (fromCapture "tags/*")

  let postTagsCtx = postCtx tags
  
  match (fromList staticFiles) $ do
    route idRoute
    compile copyFileCompiler
  
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/main.scss" $ do
    route $ setExtension "css"
    compile $ getResourceString >>= sassify

  match "notes/*" $ do
    route noteRoute
    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocTocWriter
      >>= loadAndApplyTemplate "templates/page-with-toc.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls
      >>= removeIndexHtml

  match "about.markdown" $ do
    route niceRoute
    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocTocWriter
      >>= loadAndApplyTemplate "templates/page-with-toc.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls
      >>= removeIndexHtml

  match "posts/*" $ do
    route niceDateRoute
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
      >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
      >>= relativizeUrls
      >>= removeIndexHtml

  create ["archive.html"] $ do
    route niceRoute
    compile $ do
      let archiveCtx =
            field "posts" (\_ -> postList tags recentFirst)   `mappend`
            constField "title" "Archive"                      `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
        >>= removeIndexHtml

  create ["notes.html"] $ do
    route niceRoute
    compile $ do
      let noteCtx =
            field "posts" (const noteList) `mappend`
            constField "title" "Notes"     `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/notes.html" noteCtx
        >>= loadAndApplyTemplate "templates/default.html" noteCtx
        >>= relativizeUrls
        >>= removeIndexHtml

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      loadAllSnapshots "posts/*" "content"
        >>= fmap (take 10) . recentFirst
        >>= renderRss (feedConfiguration "All posts") feedCtx
        
  match "index.html" $ do
    route idRoute
    compile $ do
      let indexCtx = field "posts" $ \_ ->
            completePostList tags $ fmap (take 5) . recentFirst

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
        >>= relativizeUrls
        >>= removeIndexHtml

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged " ++ tag

    route niceRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title <>
                listField "posts" (postCtx tags) (return posts) <>
                defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= removeIndexHtml

  match "templates/*" $ compile templateCompiler

  where pandocTocWriter = defaultHakyllWriterOptions { writerTableOfContents = True
                                                     , writerTemplate = "$if(toc)$ $toc$ $endif$\n$body$"
                                                     , writerStandalone = True }
        staticFiles = ["CNAME", "humans.txt", "robots.txt", "favicon.png", "favicon.ico"]
      

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
               [ dateField "date" "%B %e, %Y"
               , tagsField "tags" tags
               , defaultContext
               ]
--------------------------------------------------------------------------------
postList :: Tags -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags sortFilter = do
  posts   <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl (postCtx tags) posts

--------------------------------------------------------------------------------
noteList ::  Compiler String
noteList = do
  posts   <- loadAll "notes/*"
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl defaultContext posts
  
--------------------------------------------------------------------------------
-- | Returns a list of post bodies
completePostList :: Tags -> ([Item String] -> Compiler [Item String]) -> Compiler String
completePostList tags sortFilter = do
  posts   <- sortFilter =<< loadAllSnapshots "posts/*" "content"
  itemTpl <- loadBody "templates/post-with-link.html"
  applyTemplateList itemTpl (postCtx tags) posts

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
-- | Turns notes/post.html into /post/index.html
noteRoute :: Routes
noteRoute = gsubRoute "notes/" (const "") `composeRoutes` niceRoute

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
               >>= return . fmap compressCss

--------------------------------------------------------------------------------
-- | Feeds

feedCtx :: Context String
feedCtx = mconcat
          [ bodyField "description"
          , defaultContext
          ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
                          { feedTitle       = "Abizern.org - " ++ title
                          , feedDescription = "The developer and personal blog of Abizer Nasir, a freelance iOS and OS X developer in London, UK"
                          , feedAuthorName  = "Abizer Nasir"
                          , feedAuthorEmail = "abizern@abizern.org"
                          , feedRoot        = "http://abizern.org"
                          }
