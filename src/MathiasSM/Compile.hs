module MathiasSM.Compile (runPandoc, finish) where

import Control.Monad ((<=<))
import Data.List (find)
import Hakyll (
  Compiler,
  Context,
  Item,
  defaultHakyllReaderOptions,
  defaultHakyllWriterOptions,
  loadAndApplyTemplate,
  relativizeUrls,
  withTags, renderPandocWithTransform,
 )
import MathiasSM.CleanURL (cleanIndexHtmls, cleanIndexUrls)
import Text.HTML.TagSoup (Tag (TagOpen))
import Text.Pandoc.Options (MathMethod (MathJax), writerMathMethod)
import Text.Pandoc (Inline(Link, Str, RawInline))
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T


-- | Custom configuration for Pandoc
runPandoc :: Item String -> Compiler (Item String)
runPandoc = titleToAlt <=< renderPandocWithTransform
    defaultHakyllReaderOptions
    writerOptions
    transforms 
  where
    transforms = walk processRubyText
    writerOptions =
      defaultHakyllWriterOptions
        { writerMathMethod = MathJax ""
        }


-- | Factors out the final common default steps for basically all pages
finish :: Context String -> Item String -> Compiler (Item String)
finish context item =
  loadAndApplyTemplate defaultTemplate context item
    >>= relativizeUrls
    >>= cleanIndexUrls
    >>= cleanIndexHtmls
 where
  defaultTemplate = "templates/site.html"


{- | Set the @alt@ of each @img@ tag to the text in its @title@ and
remove the @title@ attribute altogether.

Pandoc doesn't support specifying alt tags and captions separately,
but it does support specifying an img tag's title. This hack lets
me repurpose the title functionality to specify alt tags instead.

Taken from
https://github.com/TikhonJelvis/website/blob/master/website.hs
-}
titleToAlt :: Item String -> Compiler (Item String)
titleToAlt item = pure $ withTags fixImg <$> item
 where
  fixImg (TagOpen "img" attributes) = TagOpen "img" $ swapTitle attributes
  fixImg other = other

  swapTitle attributes = ("alt", titleText) : clean attributes
   where
    clean = filter $ not . oneOf ["alt", "title"]
    titleText = case find (oneOf ["title"]) attributes of
      Just ("title", t) -> t
      _ -> ""
    oneOf atts (att, _) = att `elem` atts


{- | Change special syntax (overloading Links) into ruby-annotated text

Ex. `[飯](-はん)` into `<ruby lang=jp>飯<rt>はん</rt></ruby>`

NOTE: `jp` is hardcoded into the ruby tag "just in case"

NOTE: Uses RawInline since there's no native AST representation
-}
processRubyText :: Inline -> Inline
processRubyText x@(Link _ [Str kanji] (src,_)) =
  case T.uncons src of
    Just ('-', ruby) -> RawInline "html" $
      "<ruby lang=jp>" <> kanji <> "<rp>(</rp><rt>" <> ruby <> "</rt><rp>)</rp></ruby>"
    _ -> x
processRubyText x = x
