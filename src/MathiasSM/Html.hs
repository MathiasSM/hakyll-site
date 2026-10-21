-- | Small post-render HTML transforms, applied once a page is fully assembled
module MathiasSM.Html (readonlyCheckboxes) where

import Data.List (isInfixOf, words)
import Data.Maybe (fromMaybe)
import Hakyll.Web.Html (withTagList)
import Text.HTML.TagSoup (Tag (TagClose, TagOpen))

{- | Makes every task-list checkbox read-only, unless it sits inside an
`interactive`-classed element.

Pandoc renders `- [ ]` / `- [x]` as `<input type="checkbox">`; on a static page
those invite interaction that can't be persisted. This adds `aria-readonly` and
drops each one from the tab order, and the stylesheet stops pointer events on
them (and their `<label>`). A checkbox inside `<div class="interactive">` is
left editable.
-}
readonlyCheckboxes :: String -> String
readonlyCheckboxes html
    | "checkbox" `isInfixOf` html = withTagList (mark []) html
    | otherwise = html
  where
    -- The interactivity of each open `<div>`, innermost first, so a nested
    -- `<div>` can't break out of an `interactive` ancestor.
    mark :: [Bool] -> [Tag String] -> [Tag String]
    mark _ [] = []
    mark stack (t@(TagOpen "div" attrs) : rest) =
        t : mark ((interactiveDiv attrs || current stack) : stack) rest
    mark (_ : stack') (t@(TagClose "div") : rest) = t : mark stack' rest
    mark stack (t@(TagOpen "input" attrs) : rest)
        | lookup "type" attrs == Just "checkbox" && not (current stack) =
            TagOpen "input" (addIfMissing ("tabindex", "-1") (addIfMissing ("aria-readonly", "true") attrs)) : mark stack rest
        | otherwise = t : mark stack rest
    mark stack (t : rest) = t : mark stack rest

    current [] = False
    current (b : _) = b

    interactiveDiv attrs = "interactive" `elem` words (fromMaybe "" (lookup "class" attrs))

    addIfMissing attr attrs
        | any ((== fst attr) . fst) attrs = attrs
        | otherwise = attrs ++ [attr]
