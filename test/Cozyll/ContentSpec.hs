-- | Tests for the front-matter queries and aliases
module Cozyll.ContentSpec (tests) where

import Cozyll.Config (Env (..))
import Cozyll.Content (isHidden, parseAliasesYaml)
import Data.Aeson (Value (Bool))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap qualified as KM
import Data.Either (isLeft)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Content"
        [ isHiddenTests
        , aliasTests
        ]

-- Draft visibility -----------------------------------------------------------

isHiddenTests :: TestTree
isHiddenTests =
    testGroup
        "isHidden"
        [ testCase "hides a draft in prod" $
            isHidden Prod draftMetadata @?= True
        , testCase "shows a draft in dev" $
            isHidden Dev draftMetadata @?= False
        , testCase "shows a non-draft in prod" $
            isHidden Prod nonDraftMetadata @?= False
        ]
  where
    draftMetadata = KM.singleton (fromString "draft") (Bool True)
    nonDraftMetadata = KM.singleton (fromString "title") (Bool False)

-- Aliases --------------------------------------------------------------------

aliasTests :: TestTree
aliasTests =
    testGroup
        "parseAliasesYaml"
        [ testCase "reads a list" $
            parseAliasesYaml "aliases: [/old, /older/place]" @?= Right ["/old", "/older/place"]
        , testCase "reads a single string" $
            parseAliasesYaml "aliases: /old" @?= Right ["/old"]
        , testCase "is empty when there are none" $
            parseAliasesYaml "title: x" @?= Right []
        , testCase "rejects an alias without a leading slash" $
            parseAliasesYaml "aliases: [old]" @?= Left ["aliases: \"old\" must start with / (like /old-name)"]
        , testCase "rejects the home page" $
            parseAliasesYaml "aliases: [/]" @?= Left ["aliases: / is the home page, it can't be an alias"]
        , testCase "rejects something that isn't text" $
            assertBool "expected parsing to fail" (isLeft (parseAliasesYaml "aliases: 3"))
        ]
