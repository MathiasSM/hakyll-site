{- | Unit test entrypoint: the spec modules only. The golden (snapshot) tests
live in @GoldenMain.hs@ and are run as a separate test suite.
-}
module Main (main) where

import MathiasSM.AccessibilitySpec qualified as Accessibility
import MathiasSM.CleanURLSpec qualified as CleanURL
import MathiasSM.ContentSpec qualified as Content
import MathiasSM.DataSpec qualified as Data
import MathiasSM.FeedSpec qualified as Feed
import MathiasSM.HtmlSpec qualified as Html
import MathiasSM.LanguageSpec qualified as Language
import MathiasSM.MetadataSpec qualified as Metadata
import MathiasSM.NavSpec qualified as Nav
import MathiasSM.PostSpec qualified as Post
import MathiasSM.ProjectSpec qualified as Project
import MathiasSM.RedirectsSpec qualified as Redirects
import MathiasSM.SocialSpec qualified as Social
import MathiasSM.ThemeSpec qualified as Theme
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
            , Feed.tests
            , Html.tests
            , Language.tests
            , Metadata.tests
            , Nav.tests
            , Post.tests
            , Project.tests
            , Redirects.tests
            , Social.tests
            , Theme.tests
            ]
        )
