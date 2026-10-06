module MathiasSM.Tsv (Row, parseTsv, rowContext, isYes) where

import Hakyll (Context (Context), ContextField (StringField), Item (itemBody), noResult)

-- | A table row as (column name, cell) pairs
type Row = [(String, String)]

{- | Parses tab-separated text. The first line is the header; blank lines and
lines starting with `#` are skipped.
-}
parseTsv :: String -> [Row]
parseTsv text = case filter relevant (lines text) of
  [] -> []
  (header : rows) -> map (zip (cells header) . cells) rows
 where
  relevant l = not (null l) && take 1 l /= "#"
  cells s = case break (== '\t') s of
    (cell, []) -> [cell]
    (cell, _ : rest) -> cell : cells rest

-- | Exposes every column of a row as a context field
rowContext :: Context Row
rowContext = Context $ \key _ item ->
  maybe (noResult $ "No column " ++ key) (return . StringField) (lookup key $ itemBody item)

-- | Reads a boolean cell (yes/true/1)
isYes :: String -> Row -> Bool
isYes column row = lookup column row `elem` map Just ["yes", "true", "1"]
