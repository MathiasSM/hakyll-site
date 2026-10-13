-- | Custom Pandoc filters, like the ruby-text transform
module MathiasSM.Filters (processRubyText) where

import Data.Text qualified as T
import Text.Pandoc (Inline (Link, RawInline, Str))

{- | Change special syntax (overloading Links) into ruby-annotated text

Ex. @[飯](-はん)@ into @<ruby lang=jp>飯<rt>はん</rt></ruby>@

NOTE: @jp@ is hardcoded into the ruby tag /just in case/.

NOTE: Uses "RawInline" since there's no native AST representation
-}
processRubyText :: Inline -> Inline
processRubyText x@(Link _ [Str kanji] (src, _)) =
    case T.uncons src of
        Just ('-', ruby) ->
            RawInline "html" $
                "<ruby lang=jp>" <> kanji <> "<rp>(</rp><rt>" <> ruby <> "</rt><rp>)</rp></ruby>"
        _ -> x
processRubyText x = x
