-- | Build-time checks that content is consistent across files
module MathiasSM.Validate (ensureContentExists, validateHobbyPages, missingHobbyPages) where

import Control.Monad (filterM, unless)
import Data.List (intercalate)
import Hakyll (Rules, getAllMetadata, preprocess, toFilePath)
import MathiasSM.Config (contentDir, hobbiesTable, postsPattern, requiredContent)
import MathiasSM.Content (isDraft)
import MathiasSM.Metadata (Key (Path), lookupKey)
import MathiasSM.Tsv (Row, parseTsv)
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

{- | Fails the build, once, if any hobby (shown or not) lacks a published blog post with `path: <href>`

Runs when the rules are set up, not for every page that lists the hobbies.
-}
validateHobbyPages :: Rules ()
validateHobbyPages = do
  table <- preprocess $ readFile $ toFilePath hobbiesTable
  posts <- getAllMetadata $ postsPattern "blog"
  let paths = [path | (_, metadata) <- posts, not (isDraft metadata), Just path <- [lookupKey Path metadata]]
      missing = missingHobbyPages (parseTsv table) paths
  unless (null missing) $
    preprocess $
      ioError $
        userError $
          "hobbies.tsv: no post in " ++ contentDir ++ "/blog with a matching `path:` for: "
            ++ intercalate ", " missing

-- | The `href` of every hobby that no post `path` matches
missingHobbyPages :: [Row] -> [FilePath] -> [FilePath]
missingHobbyPages hobbies paths = [href | row <- hobbies, Just href <- [lookup "href" row], href `notElem` paths]
