module MathiasSM.CleanURL (cleanRoute, pathRoute, cleanIndexUrls, cleanIndexHtmls) where

import Data.List (isSuffixOf)
import Hakyll (Compiler, Item, Metadata, Routes, composeRoutes, constRoute, customRoute, gsubRoute, replaceAll, toFilePath, withUrls)
import MathiasSM.Metadata (Key (Path), lookupKey)
import System.FilePath.Posix (takeBaseName, takeDirectory, takeExtension, (</>))

-- | Makes routes be something/index.html instead of just something.html
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute `composeRoutes` gsubRoute "^./" (const "")
 where
  createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
   where
    p = toFilePath ident

{- | Route from the `path:` front matter: `/contact` becomes `contact/index.html`,
while a path with an extension (like `/404.html`) is kept as is.
-}
pathRoute :: Metadata -> Routes
pathRoute metadata = case lookupKey Path metadata of
  Nothing -> customRoute $ \ident -> error $ "Missing `path` in " ++ toFilePath ident
  Just path
    | null (takeExtension path) -> withoutSlash `composeRoutes` cleanRoute
    | otherwise -> withoutSlash
   where
    withoutSlash = constRoute path `composeRoutes` gsubRoute "^/" (const "")

-- | Cleans all URLs
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

-- | Cleans all instances of /index, that might not be a recognized as a URL
cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll indexPattern replacement)
 where
  indexPattern = "/index.html"
  replacement = const ""

-- | Strips a URL of its index.html suffix
cleanIndex :: String -> String
cleanIndex url
  | idx == url = "/"
  | idx `isSuffixOf` url = take (length url - length idx) url
  | otherwise = url
 where
  idx = "/index.html"
