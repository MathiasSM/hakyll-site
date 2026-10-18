-- | Shared helpers for decoding the site's YAML documents into typed values
module MathiasSM.Yaml (
    decodeYaml,
    parseYamlList,
    Check (..),
    required,
    optional,
    enumFromJSON,
) where

import Data.Aeson (FromJSON (parseJSON), Object, Value (Null), withText)
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.List (intercalate, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml (decodeEither', prettyPrintParseException)
import MathiasSM.Metadata (Key, keyName)
import Prelude hiding (lookup)

-- | Decodes a YAML document, reporting the parse problems found
decodeYaml :: (FromJSON a) => String -> Either [String] a
decodeYaml = first (\e -> [prettyPrintParseException e]) . decodeEither' . T.encodeUtf8 . T.pack

-- | Decodes a top-level YAML list of objects, parsing each one and prefixing its problems by index
parseYamlList :: String -> (Object -> Either [String] a) -> String -> Either [String] [a]
parseYamlList what parseOne text = do
    objects <- decodeYaml text
    let numbered = zip [0 :: Int ..] (map parseOne objects)
        problems = concat [map (prefix index) ps | (index, Left ps) <- numbered]
        parsed = [x | (_, Right x) <- numbered]
    if null problems then Right parsed else Left problems
  where
    prefix index problem = what ++ " #" ++ show index ++ ": " ++ problem

-- | Accepts exactly the spellings @render@ produces
enumFromJSON :: (Enum a, Bounded a) => String -> (a -> String) -> Value -> Parser a
enumFromJSON what render = withText what $ \t ->
    case [x | x <- [minBound .. maxBound], render x == T.unpack t] of
        x : _ -> pure x
        [] ->
            fail $
                "unknown "
                    ++ what
                    ++ " \""
                    ++ T.unpack t
                    ++ "\" (expected "
                    ++ intercalate ", " (map render [minBound .. maxBound])
                    ++ ")"

-- | Like 'Either', but combining failures instead of stopping at the first
newtype Check a = Check {runCheck :: Either [String] a}

instance Functor Check where
    fmap f (Check r) = Check (fmap f r)

instance Applicative Check where
    pure = Check . Right
    Check (Left a) <*> Check (Left b) = Check (Left (a ++ b))
    Check (Left a) <*> Check _ = Check (Left a)
    Check (Right f) <*> Check r = Check (fmap f r)

required :: (FromJSON a) => Object -> Key -> Check a
required o key = case lookup (fromString $ keyName key) o of
    Nothing -> Check $ Left [keyName key ++ ": missing"]
    Just Null -> Check $ Left [keyName key ++ ": missing"]
    Just v -> Check $ decodeField key v

optional :: (FromJSON a) => Object -> Key -> Check (Maybe a)
optional o key = case lookup (fromString $ keyName key) o of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just v -> Check $ Just <$> decodeField key v

decodeField :: (FromJSON a) => Key -> Value -> Either [String] a
decodeField key = first (\e -> [keyName key ++ ": " ++ clean e]) . parseEither parseJSON
  where
    clean e = fromMaybe e (stripPrefix "Error in $: " e)
