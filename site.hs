--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
rules = do
  match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

  match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  match "posts/*" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  create ["archive.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Archives"            `mappend`
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls

  match "index.html" $ do
      route idRoute
      compile $ do
          latestPost : _ <- recentFirst =<< loadAllSnapshots "posts/*" "content"
          let latestCtx =
                    constField "latest" (itemBody latestPost) `mappend`
                    defaultContext

          getResourceBody
              >>= applyAsTemplate latestCtx
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

main :: IO ()
main = hakyll rules

-- run hakyll clean and hakyll watch
-- for use with ghcid
ghcid :: IO ()
ghcid = do
          hackleCmd Clean
          hackleCmd $ Watch "localhost" 8000 False
          return ()
  where
    hackleCmd command = hakyllWithExitCodeAndArgs defaultConfiguration (Options False command) rules

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
