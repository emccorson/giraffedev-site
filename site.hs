--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Maybe (fromMaybe)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           Data.Time.Clock (utctDay)
import           Data.Time.Calendar (toGregorian, fromGregorian)
import           Data.Time.Format (formatTime)
import           Data.List (groupBy)
import           System.FilePath.Posix ((</>), takeBaseName)
import           Text.Regex.TDFA ((=~))

import Debug.Trace

--------------------------------------------------------------------------------
rules = do
  match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

  match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

  match "posts/*" $ do
      route cleanRoute
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls
          >>= cleanUrls

  create ["archive.html"] $ do
      route cleanRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          grouped <- groupPosts posts

          -- group posts by year and then by month
          let indexedPostCtx =
                  listField "years" (
                    field "year" (return . show . fst . itemBody) <>
                    listFieldWith "months" (
                      field "month" (return . monthToString . fst . itemBody) <>
                      listFieldWith "posts" (
                        dateField "date" "%e" <>
                        globalCtx
                      ) (return . snd . itemBody)
                    ) (mapM makeItem . snd . itemBody)
                  ) (mapM makeItem grouped)

          let archiveCtx =
                  indexedPostCtx <>
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Archives"            `mappend`
                  globalCtx

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" globalCtx
              >>= relativizeUrls
              >>= cleanUrls

  match "contact.md" $ do
      route cleanRoute
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" globalCtx
          >>= relativizeUrls
          >>= cleanUrls

  match "index.html" $ do
      route idRoute
      compile $ do
          latestPost : _ <- recentFirst =<< loadAllSnapshots "posts/*" "content"
          postTitle <- getMetadataField (itemIdentifier latestPost) "title"
          let latestCtx =
                    constField "latest" (itemBody latestPost) `mappend`
                    constField "title" (fromMaybe "" postTitle) `mappend`
                    globalCtx

          getResourceBody
              >>= applyAsTemplate latestCtx
              >>= loadAndApplyTemplate "templates/default.html" latestCtx
              >>= relativizeUrls
              >>= cleanUrls

  match "templates/*" $ compile templateBodyCompiler

main :: IO ()
main = hakyll rules

-- run hakyll clean and hakyll watch
-- for use with ghcid
ghcid :: IO ()
ghcid = do
          hakyllCmd Clean
          hakyllCmd $ Watch "localhost" 8000 False
          return ()
  where
    hakyllCmd command = hakyllWithExitCodeAndArgs defaultConfiguration (Options False command) rules

--------------------------------------------------------------------------------
globalCtx :: Context String
globalCtx =
    constField "site" "giraffedev" `mappend`
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    globalCtx


type Year = Integer
type Month = Int

-- group posts by year and then by month
groupPosts :: [Item String] -> Compiler [(Year, [(Month, [Item String])])]
groupPosts xs = mapM makeTuple xs >>= \tupleList ->
                    let
                      yearGrouped = group tupleList
                      yearIndexed = map index yearGrouped
                      monthIndexed = map (\(y, mi) -> (y, (map index . group) mi)) yearIndexed
                    in return monthIndexed
  where

    makeTuple :: Item String -> Compiler (Year, (Month, Item String))
    makeTuple i = do
      (year, month, _) <- return . toGregorian . utctDay =<< getItemUTC defaultTimeLocale (itemIdentifier i)
      return (year, (month, i))

    group :: Eq a => [(a, b)] -> [[(a, b)]]
    group = groupBy (\(x, _) (y, _) -> x == y)

    index :: [(a, b)] -> (a, [b])
    index (x:xs) = (fst x, map snd (x:xs))

-- e.g. 3 -> "March"
monthToString :: Month -> String
monthToString m = formatTime defaultTimeLocale "%B" (fromGregorian 1 m 1)

-- use e.g. post-name/index.html instead of 2022-04-04-post-name.html as route
-- allows use of /post-name as URL since this will normally default to post-name/index.html
cleanRoute :: Routes
cleanRoute = customRoute makeRoute
  where
    stripDate :: String -> String
    stripDate str = let (emptyIfMatch, _, title) = str =~ ("^[0-9]{4}-[0-9]{2}-[0-9]{2}-" :: String) :: (String, String, String)
                    in if emptyIfMatch == "" then title else str

    makeRoute identifier = stripDate (takeBaseName (toFilePath identifier)) </> "index.html"

-- strip "/index.html" suffix if it exists; otherwise, return URL unchanged.
cleanUrls :: Item String -> Compiler (Item String)
cleanUrls = return . fmap (withUrls stripIndex)
  where
    stripIndex :: String -> String
    stripIndex url = let (stripped, _, _) = url =~ ("/index.html$" :: String) :: (String, String, String)
                     in stripped
