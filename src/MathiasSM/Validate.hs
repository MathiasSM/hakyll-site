-- | Build-time checks that content is consistent across files
module MathiasSM.Validate (ensureHobbyPagesExist) where

import Control.Monad (unless)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Hakyll (Compiler, Item (itemBody), getMatches, getMetadata, load)
import MathiasSM.Config (hobbiesTable, postsPattern)
import MathiasSM.Metadata (Key (Path), lookupKey)
import MathiasSM.Tsv (parseTsv)

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
      "hobbies.tsv: no post in data/posts/blog with a matching `path:` for: "
        ++ intercalate ", " missing
