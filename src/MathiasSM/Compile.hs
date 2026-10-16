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
    getMetadata,
    getUnderlying,
    loadAndApplyTemplate,
    relativizeUrls,
    renderPandocWithTransform,
 )
import MathiasSM.Accessibility (checkAccessibility)
import MathiasSM.CleanURL (cleanIndexHtmls, cleanIndexUrls)
import MathiasSM.Config (siteTemplate)
import MathiasSM.Metadata (Key (TOC), lookupKey)
import Text.Pandoc (Inline (Link, RawInline, Str))
import Text.Pandoc.Options (MathMethod (MathJax), writerMathMethod, writerTableOfContents)
import Text.Pandoc.Walk (walk)

-- TODO(citeproc): Citations are left unprocessed today -- `Ext_citations` is on
-- via `pandocExtensions`, but no citeproc/bibliography is configured, so `@key`
-- in prose renders as a bare `<span class="citation">`. If a bibliography (or
-- citeproc) is ever added, literal `@` in content must be escaped as `\@`, or
-- those fragments will silently turn into citations.

-- | Custom configuration for Pandoc
runPandoc :: Item String -> Compiler (Item String)
runPandoc item = do
    metadata <- getUnderlying >>= getMetadata
    let toc = lookupKey TOC metadata == Just "true"
        opts =
            writerOptions
                { writerTableOfContents = toc
                }
    renderPandocWithTransform defaultHakyllReaderOptions opts transforms item
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
