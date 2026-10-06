module MathiasSM.Rules.Redirects (processRedirects, Entry (..), planRedirects) where

import Data.Either (fromLeft)
import Data.List (tails)
import Hakyll (
  Rules,
  createRedirects,
  fromFilePath,
  getAllMetadata,
  preprocess,
  toFilePath,
  version,
 )
import MathiasSM.CleanURL (outputPath)
import MathiasSM.Config (pagesPattern, postGroups, postsPattern)
import MathiasSM.Content (aliasesOf, isDraft)
import MathiasSM.Metadata (Key (Path), lookupKey)

-- | A published page or post, with the old paths that should lead to it
data Entry = Entry
  { entryFile :: FilePath
  , entryPath :: Maybe FilePath
  , entryAliases :: [FilePath]
  }
  deriving (Eq, Show)

{- | Builds a redirect page for every `aliases:` of every published page and post

Invalid aliases fail the build.
-}
processRedirects :: Rules ()
processRedirects = do
  items <- concat <$> mapM getAllMetadata (pagesPattern "*" : map postsPattern postGroups)
  let parsed = [(toFilePath ident, metadata, aliasesOf metadata) | (ident, metadata) <- items, not (isDraft metadata)]
      entries = [Entry file (lookupKey Path metadata) aliases | (file, metadata, Right aliases) <- parsed]
      planned = planRedirects entries
      problems =
        [file ++ ": " ++ problem | (file, _, Left found) <- parsed, problem <- found]
          ++ fromLeft [] planned
  case planned of
    Right redirects | null problems -> version "redirects" $ createRedirects [(fromFilePath out, target) | (out, target) <- redirects]
    _ -> preprocess $ ioError $ userError $ "Invalid aliases:\n" ++ unlines (map ("  " ++) problems)

{- | The redirects to build, as (output file, target path), or every problem found:
an alias that is its own path, the path of another page, or claimed twice
-}
planRedirects :: [Entry] -> Either [String] [(FilePath, FilePath)]
planRedirects entries
  | null problems = Right [(outputPath alias, target) | (_, alias, Just target) <- claims]
  | otherwise = Left problems
 where
  claims = [(file, alias, path) | Entry file path aliases <- entries, alias <- aliases]
  published = [(file, path) | Entry file (Just path) _ <- entries]
  same a b = outputPath a == outputPath b
  problems =
    [file ++ ": aliases need a `path:` to point to" | (file, _, Nothing) <- claims]
      ++ [file ++ ": alias " ++ alias ++ " is its own path" | (file, alias, Just path) <- claims, same alias path]
      ++ [file ++ ": alias " ++ alias ++ " is the path of " ++ other | (file, alias, _) <- claims, (other, path) <- published, other /= file, same alias path]
      ++ [ file ++ ": alias " ++ alias ++ " is also claimed by " ++ other
         | (file, alias, _) : later <- tails claims
         , (other, alias', _) <- later
         , same alias alias'
         ]
