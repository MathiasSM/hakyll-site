-- | Small post-render HTML transforms, applied once a page is fully assembled
module Cozyll.Html (readonlyCheckboxes) where

import Data.List (isInfixOf, words)
import Data.Maybe (fromMaybe)
import Hakyll.Web.Html (withTagList)
import Text.HTML.TagSoup (Tag (TagClose, TagOpen))

{- | Makes every pandoc task-list checkbox read-only, unless it sits inside an
`interactive`-classed element.

Pandoc renders `- [ ]` / `- [x]` as `<input type="checkbox">` inside a
`<ul class="task-list">`. On a static page those invite interaction that can't
be persisted, so this adds `aria-readonly` and drops each one from the tab
order, and the stylesheet stops pointer events on them (and their `<label>`).
A checkbox inside `<div class="interactive">` is left editable, as is any
checkbox outside a task list (e.g. a hand-authored control).
-}
readonlyCheckboxes :: String -> String
readonlyCheckboxes html
    | "checkbox" `isInfixOf` html = withTagList (walk []) html
    | otherwise = html
  where
    -- Each open `<div>`/`<ul>` pushes the current (interactive, task-list) flags.
    walk :: [(Bool, Bool)] -> [Tag String] -> [Tag String]
    walk _ [] = []
    walk stack (t@(TagOpen "div" attrs) : rest) =
        let (interactive, taskList) = top stack
         in t : walk ((interactiveDiv attrs || interactive, taskList) : stack) rest
    walk stack (t@(TagOpen "ul" attrs) : rest) =
        let (interactive, taskList) = top stack
         in t : walk ((interactive, taskListUl attrs || taskList) : stack) rest
    walk (_ : stack') (t@(TagClose name) : rest)
        | name == "div" || name == "ul" = t : walk stack' rest
    walk stack (t@(TagOpen "input" attrs) : rest)
        | isCheckbox attrs && taskList && not interactive =
            TagOpen "input" (addIfMissing ("tabindex", "-1") (addIfMissing ("aria-readonly", "true") attrs)) : walk stack rest
        | otherwise = t : walk stack rest
      where
        (interactive, taskList) = top stack
    walk stack (t : rest) = t : walk stack rest

    top [] = (False, False)
    top (scope : _) = scope

    interactiveDiv attrs = "interactive" `elem` words (fromMaybe "" (lookup "class" attrs))
    taskListUl attrs = "task-list" `elem` words (fromMaybe "" (lookup "class" attrs))
    isCheckbox attrs = lookup "type" attrs == Just "checkbox"

    addIfMissing attr attrs
        | any ((== fst attr) . fst) attrs = attrs
        | otherwise = attrs ++ [attr]
