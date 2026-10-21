{- | Unit test entrypoint: the spec modules only. The golden (snapshot) tests
live in @GoldenMain.hs@ and are run as a separate test suite.
-}
module Main (main) where

import MathiasSM.AccessibilitySpec qualified as Accessibility
import MathiasSM.CleanURLSpec qualified as CleanURL
import MathiasSM.ContentSpec qualified as Content
import MathiasSM.DataSpec qualified as Data
import MathiasSM.FeedsSpec qualified as Feeds
import MathiasSM.HtmlSpec qualified as Html
import MathiasSM.MetadataSpec qualified as Metadata
import MathiasSM.RedirectsSpec qualified as Redirects
import MathiasSM.YamlSpec qualified as Yaml
import Test.Tasty (defaultIngredients, defaultMainWithIngredients, testGroup)
import Test.Tasty.Runners.AntXML (antXMLRunner)

main :: IO ()
main =
    defaultMainWithIngredients
        (antXMLRunner : defaultIngredients)
        ( testGroup
            "unit"
            [ Accessibility.tests
            , CleanURL.tests
            , Content.tests
            , Data.tests
            , Feeds.tests
            , Html.tests
            , Metadata.tests
            , Redirects.tests
            , Yaml.tests
            ]
        )
