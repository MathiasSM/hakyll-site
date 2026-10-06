-- | Build-time checks that content is consistent across files
module MathiasSM.Validate (ensureContentExists, ensureHobbyPagesExist) where

import Control.Monad (filterM, unless)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Hakyll (Compiler, Item (itemBody), getMatches, getMetadata, load)
import MathiasSM.Config (contentDir, hobbiesTable, postsPattern, requiredContent)
import MathiasSM.Metadata (Key (Path), lookupKey)
import MathiasSM.Tsv (parseTsv)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)

{- | Stops before building if the content repo is missing, or lacks a file the site needs

Hakyll would otherwise build whatever it finds, silently leaving out missing pages.
-}
ensureContentExists :: IO ()
ensureContentExists = do
  present <- doesDirectoryExist contentDir
  unless present $
    die $ contentDir ++ "/ not found: clone the content repo there (see README)"
  missing <- filterM (fmap not . doesFileExist) requiredContent
  unless (null missing) $
    die $ "Missing required content:\n" ++ unlines (map ("  " ++) missing)

-- | Fails the build if any hobby (shown or not) lacks a blog post with `path: <href>`
ensureHobbyPagesExist :: Compiler ()
ensureHobbyPagesExist = do
  table <- load hobbiesTable
  postIds <- getMatches $ postsPattern "blog"
  paths <- mapMaybe (lookupKey Path) <$> mapM getMetadata postIds
  let hrefs = [h | row <- parseTsv $ itemBody table, Just h <- [lookup "href" row]]
      missing = filter (`notElem` paths) hrefs
  unless (null missing) $
    fail $
      "hobbies.tsv: no post in content/blog with a matching `path:` for: "
        ++ intercalate ", " missing
