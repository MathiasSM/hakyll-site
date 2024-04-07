{-# LANGUAGE OverloadedStrings #-}

module MathiasSM.Rules.Redirects (processRedirects) where

import Hakyll ( Rules, version, createRedirects, Identifier )

-- | Builds all necessary redirections
processRedirects :: Rules ()
processRedirects = version "redirects" $ createRedirects brokenLinks

-- | List of broken/old link redirections
brokenLinks :: [(Identifier, String)]
brokenLinks =
  [ ("about/index.html", "/")
  ]
