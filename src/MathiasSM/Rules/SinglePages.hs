{-# LANGUAGE OverloadedStrings #-}

module MathiasSM.Rules.SinglePages (processKnownPage, processKnownPage') where

import Data.String (fromString)
import Hakyll (
  Compiler,
  Context,
  Identifier,
  Item,
  Rules,
  compile,
  applyAsTemplate,
  composeRoutes,
  getMetadata,
  getUnderlying,
  lookupString,
  composeRoutes,
  constRoute,
  getResourceString,
  loadAndApplyTemplate,
  match,
  route,
 )
import MathiasSM.CleanURL (cleanRoute)
import MathiasSM.Compile (finish, runPandoc)
import MathiasSM.Context (minimalCtx, navStateContext)
import Control.Monad ((>=>))

preTemplates :: [Identifier]
preTemplates = ["templates/minimal.html"]

postTemplates :: [Identifier]
postTemplates = ["templates/as-page.html"]

knownPagePatternString :: String -> String
knownPagePatternString pageName = "data/pages/" ++ pageName ++ ".*"

-- | Filters named routes
finalPageRoute :: String -> String
finalPageRoute "about" = ""
finalPageRoute "404" = "404.html"
finalPageRoute pageName = pageName

-- | Chains multiple templates into a single monadic action
templateSteps :: Context String -> [Identifier] -> [Item String -> Compiler (Item String)]
templateSteps ctx = map (`loadAndApplyTemplate` ctx)

applyMyTemplates :: Context String -> [Identifier] -> Item String -> Compiler (Item String)
applyMyTemplates ctx extraTemplates =
  let templates = concat [preTemplates, extraTemplates, postTemplates]
      steps = templateSteps ctx templates
  in foldl (>=>) return steps

-- | Processes a given standalone page
processKnownPage :: String -> [Identifier] -> Rules ()
processKnownPage = processKnownPage' True (return minimalCtx)

-- | Processes a given standalone page
processKnownPage' :: Bool -> Compiler (Context String) -> String -> [Identifier] -> Rules ()
processKnownPage' mustCleanRoute getCtx pageName extraTemplates = match pagePattern $ do
  route $ if mustCleanRoute
            then constRoute pageRoute `composeRoutes` cleanRoute
            else constRoute pageRoute
  compile $ do
    ctx <- getCtx
    templated <- isTemplated
    getResourceString
      >>= (if templated then applyAsTemplate ctx else return)
      >>= runPandoc
      >>= applyMyTemplates ctx extraTemplates
      >>= finish (navStateContext pageName <> ctx)
 where
  pagePattern = fromString $ knownPagePatternString pageName
  pageRoute = finalPageRoute pageName
  -- Pages with `templated: true` may use template syntax in their body
  isTemplated = do
    metadata <- getUnderlying >>= getMetadata
    return $ lookupString "templated" metadata == Just "true"
