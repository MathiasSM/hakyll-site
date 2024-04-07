{-# LANGUAGE OverloadedStrings #-}

module MathiasSM.Compile (runPandoc, finish) where

import Control.Monad ((<=<))
import Data.List (find)
import Hakyll
import MathiasSM.CleanURL (cleanIndexHtmls, cleanIndexUrls)
import Text.HTML.TagSoup (Tag (TagOpen))
import Text.Pandoc.Options (HTMLMathMethod (MathJax), writerHTMLMathMethod)

-- | Custom configuration for Pandoc
runPandoc :: Item String -> Compiler (Item String)
runPandoc = titleToAlt <=< renderPandocWith defaultHakyllReaderOptions pandocOptions
  where
    pandocOptions =
        defaultHakyllWriterOptions
            { writerHTMLMathMethod = MathJax ""
            }

-- | Refators away the final common default steps for basically all pages
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
            Just ("title", titleText) -> titleText
            _ -> ""
        oneOf atts (att, _) = att `elem` atts
