-- | Single tasty entrypoint: unit tests plus the golden (snapshot) tests.
module Main (main) where

import Control.Exception (bracket)
import Golden (buildSiteSnapshot, goldenTests, relativeFiles)
import MathiasSM.AccessibilitySpec qualified as Accessibility
import MathiasSM.CleanURLSpec qualified as CleanURL
import MathiasSM.ContentSpec qualified as Content
import MathiasSM.FeedsSpec qualified as Feeds
import MathiasSM.MetadataSpec qualified as Metadata
import MathiasSM.RedirectsSpec qualified as Redirects
import MathiasSM.TsvSpec qualified as Tsv
import MathiasSM.ValidateSpec qualified as Validate
import System.Directory (getCurrentDirectory, removePathForcibly)
import Test.Tasty (TestTree, defaultMain, testGroup)

-- TODO: add tasty-ant-xml and wire a JUnit XML ingredient into CI for prettier
-- results. Something like:
--   defaultMainWithIngredients (antXMLRunner : defaultIngredients) tests
-- | Builds the fixture site snapshot, then runs the unit and golden tests
main :: IO ()
main = do
    root <- getCurrentDirectory
    bracket (buildSiteSnapshot root) (removePathForcibly . fst) $ \(_, snapshotDir) -> do
        files <- relativeFiles snapshotDir
        defaultMain (tests root snapshotDir files)

tests :: FilePath -> FilePath -> [FilePath] -> TestTree
tests root snapshotDir files =
    testGroup
        "mathiassm"
        [ testGroup
            "unit"
            [ Accessibility.tests
            , CleanURL.tests
            , Content.tests
            , Feeds.tests
            , Metadata.tests
            , Redirects.tests
            , Tsv.tests
            , Validate.tests
            ]
        , goldenTests root snapshotDir files
        ]
