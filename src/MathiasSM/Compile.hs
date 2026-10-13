-- | Pandoc compilation and the post-processing shared by every page
module MathiasSM.Compile (runPandoc, applyTemplates, finish) where

import Control.Monad (foldM)
import Data.Text qualified as T
import Hakyll (
    Compiler,
    Context,
    Identifier,
    Item,
    defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
    loadAndApplyTemplate,
    relativizeUrls,
    renderPandocWithTransform,
 )
import MathiasSM.Accessibility (checkAccessibility)
import MathiasSM.CleanURL (cleanIndexHtmls, cleanIndexUrls)
import MathiasSM.Config (siteTemplate)
import Text.Pandoc (Inline (Link, RawInline, Str))
import Text.Pandoc.Options (MathMethod (MathJax), writerMathMethod)
import Text.Pandoc.Walk (walk)

-- | Custom configuration for Pandoc
runPandoc :: Item String -> Compiler (Item String)
runPandoc =
    renderPandocWithTransform
        defaultHakyllReaderOptions
        writerOptions
        transforms
  where
    transforms = walk processRubyText
    writerOptions =
        defaultHakyllWriterOptions
            { writerMathMethod = MathJax ""
            }

-- | Applies the given templates in order, each wrapping the previous result
applyTemplates :: Context String -> [Identifier] -> Item String -> Compiler (Item String)
applyTemplates ctx templates item = foldM (\i t -> loadAndApplyTemplate t ctx i) item templates

-- | Factors out the final common default steps for basically all pages
finish :: Context String -> Item String -> Compiler (Item String)
finish context item =
    applyTemplates context [siteTemplate] item
        >>= relativizeUrls
        >>= cleanIndexUrls
        >>= cleanIndexHtmls
        >>= checkAccessibility

{- | Change special syntax (overloading Links) into ruby-annotated text

Ex. `[飯](-はん)` into `<ruby lang=jp>飯<rt>はん</rt></ruby>`

NOTE: @jp@ is hardcoded into the ruby tag "just in case"

NOTE: Uses RawInline since there's no native AST representation
-}
processRubyText :: Inline -> Inline
processRubyText x@(Link _ [Str kanji] (src, _)) =
    case T.uncons src of
        Just ('-', ruby) ->
            RawInline "html" $
                "<ruby lang=jp>" <> kanji <> "<rp>(</rp><rt>" <> ruby <> "</rt><rp>)</rp></ruby>"
        _ -> x
processRubyText x = x
