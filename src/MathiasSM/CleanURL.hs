module MathiasSM.CleanURL (cleanRoute, cleanIndexUrls, cleanIndexHtmls) where

import Data.List (isSuffixOf)
import Hakyll (Compiler, Item, Routes, composeRoutes, customRoute, gsubRoute, replaceAll, toFilePath, withUrls)
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))

-- | Makes routes be something/index.html instead of just something.html
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute `composeRoutes` gsubRoute "^./" (const "")
 where
  createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
   where
    p = toFilePath ident

-- | Cleans all URLs
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

-- | Cleans all instances of /index, that might not be a recognized as a URL
cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll indexPattern replacement)
 where
  indexPattern = "/index.html"
  replacement = const ""

-- | Strips a URL of it's index.html suffix
cleanIndex :: String -> String
cleanIndex url
  | idx == url = "/"
  | idx `isSuffixOf` url = take (length url - length idx) url
  | otherwise = url
 where
  idx = "/index.html"
