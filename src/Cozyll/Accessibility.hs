-- | Build-time accessibility checks on the final rendered HTML of a page.
module Cozyll.Accessibility (checkAccessibility, checkHtml) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Char (isSpace)
import Data.List (group, sort)
import Data.Maybe (isNothing, mapMaybe)
import Hakyll (Compiler, Item (itemBody), getUnderlying, toFilePath)
import Text.HTML.TagSoup (
    Tag (..),
    innerText,
    isTagCloseName,
    isTagOpenName,
    parseTags,
    sections,
 )

-- | Runs every check against the given HTML and returns each problem found
checkHtml :: String -> [String]
checkHtml html = concatMap (\check -> check tags) checks
  where
    tags = parseTags html

checks :: [[Tag String] -> [String]]
checks =
    [ checkImages
    , checkTitle
    , checkLanguage
    , checkH1
    , checkHeadingOrder
    , checkEmptyHeadings
    , checkLinks
    , checkIds
    , checkMain
    ]

-- | Fails the compilation if the page fails any accessibility check
checkAccessibility :: Item String -> Compiler (Item String)
checkAccessibility item = do
    ident <- getUnderlying
    let problems = checkHtml (itemBody item)
        path = toFilePath ident
    unless (null problems) $
        throwError (map ((path ++ ": ") ++) problems)
    pure item

-- Checks ---------------------------------------------------------------------

-- | Every @img@ must have an @alt@ attribute (empty is allowed for decorative images)
checkImages :: [Tag String] -> [String]
checkImages tags =
    ["image missing an `alt` attribute" | TagOpen "img" attrs <- tags, isNothing (lookup "alt" attrs)]

{- | Exactly one non-empty document @title@ (the one in @head@).

@<title>@ elements inside inline SVGs (e.g. the site logo) are SVG accessible
names, not document titles, so they are ignored.
-}
checkTitle :: [Tag String] -> [String]
checkTitle tags =
    case innerTextsOf "title" headTags of
        [] -> ["no <title>"]
        [t]
            | all isSpace t -> ["empty <title>"]
            | otherwise -> []
        _ -> ["multiple <title> elements"]
  where
    -- The document title always lives in @head@, before the @body@ opens.
    headTags = takeWhile (not . isTagOpenName "body") tags

-- | A non-empty @lang@ on the single @html@ element
checkLanguage :: [Tag String] -> [String]
checkLanguage tags =
    case [lookup "lang" attrs | TagOpen "html" attrs <- tags] of
        [] -> ["no <html> element"]
        [Just lang]
            | not (all isSpace lang) -> []
        _ -> ["missing non-empty `lang` on <html>"]

-- | Exactly one @h1@ (its content is checked by 'checkEmptyHeadings')
checkH1 :: [Tag String] -> [String]
checkH1 tags =
    case length (filter (isTagOpenName "h1") tags) of
        0 -> ["no <h1>"]
        1 -> []
        _ -> ["multiple <h1> elements"]

-- | No skipped heading levels (e.g. @h1@ straight to @h3@)
checkHeadingOrder :: [Tag String] -> [String]
checkHeadingOrder =
    skipProblems . mapMaybe headingLevel
  where
    headingLevel (TagOpen name _) = headingLevelOf name
    headingLevel _ = Nothing

skipProblems :: [Int] -> [String]
skipProblems levels =
    [ "heading level skips from h" ++ show prev ++ " to h" ++ show cur
    | (prev, cur) <- zip levels (drop 1 levels)
    , cur > prev + 1
    ]

headingLevelOf :: String -> Maybe Int
headingLevelOf "h1" = Just 1
headingLevelOf "h2" = Just 2
headingLevelOf "h3" = Just 3
headingLevelOf "h4" = Just 4
headingLevelOf "h5" = Just 5
headingLevelOf "h6" = Just 6
headingLevelOf _ = Nothing

-- | No empty headings
checkEmptyHeadings :: [Tag String] -> [String]
checkEmptyHeadings tags =
    [ "empty <" ++ name ++ ">"
    | name <- ["h1", "h2", "h3", "h4", "h5", "h6"]
    , text <- innerTextsOf name tags
    , all isSpace text
    ]

{- | Every @a@ has an accessible name: text, `aria-label`, @title@, or an inner image @alt@
(links marked `aria-hidden="true"`, like pandoc's code-block line anchors, are skipped)
-}
checkLinks :: [Tag String] -> [String]
checkLinks tags =
    [ "link has no accessible name" ++ hrefOf attrs
    | (TagOpen "a" attrs : rest) <- sections (isTagOpenName "a") tags
    , lookup "aria-hidden" attrs /= Just "true"
    , let content = takeWhile (not . isTagCloseName "a") rest
    , not (hasAccessibleName attrs content)
    ]
  where
    hasAccessibleName attrs content =
        nonEmpty (lookup "aria-label" attrs)
            || nonEmpty (lookup "title" attrs)
            || any hasText content
            || any hasImgAlt content

    hasText (TagText t) = not (all isSpace t)
    hasText _ = False

    hasImgAlt (TagOpen "img" iattrs) = nonEmpty (lookup "alt" iattrs)
    hasImgAlt _ = False

    nonEmpty (Just s) = not (all isSpace s)
    nonEmpty Nothing = False

    hrefOf attrs = maybe "" (\h -> " (href \"" ++ h ++ "\")") (lookup "href" attrs)

-- | No duplicate `id` values
checkIds :: [Tag String] -> [String]
checkIds tags =
    [ "duplicate id: " ++ i
    | (i : _ : _) <- group (sort [i | TagOpen _ attrs <- tags, Just i <- [lookup "id" attrs]])
    ]

-- | Exactly one @main@
checkMain :: [Tag String] -> [String]
checkMain tags =
    case length (filter (isTagOpenName "main") tags) of
        0 -> ["no <main>"]
        1 -> []
        _ -> ["multiple <main> elements"]

-- Helpers --------------------------------------------------------------------

-- | The inner text of every element with the given name ("" for an empty element)
innerTextsOf :: String -> [Tag String] -> [String]
innerTextsOf name =
    map (innerText . takeWhile (not . isTagCloseName name) . drop 1)
        . sections (isTagOpenName name)
