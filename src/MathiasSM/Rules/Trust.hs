module MathiasSM.Rules.Trust (processTrust) where

import Hakyll (
  Rules,
  compile,
  create,
  idRoute,
  loadAndApplyTemplate,
  makeItem,
  route,
 )
import MathiasSM.Context (minimalCtx)

-- | Builds trust.txt from the socials table
processTrust :: Rules ()
processTrust = create [".well-known/trust.txt"] $ do
  route idRoute
  compile $
    makeItem "" >>= loadAndApplyTemplate "templates/trust.txt" minimalCtx
