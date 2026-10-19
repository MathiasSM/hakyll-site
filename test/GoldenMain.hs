{- | Golden (snapshot) test entrypoint. Unit tests live in @SpecMain.hs@ and
are run as a separate test suite.
-}
module Main (main) where

import Control.Exception (bracket)
import Golden (buildSiteSnapshot, goldenTests, relativeFiles)
import System.Directory (getCurrentDirectory, removePathForcibly)
import Test.Tasty (defaultMain)

-- | Builds the fixture site snapshot, then runs the golden tests
main :: IO ()
main = do
    root <- getCurrentDirectory
    bracket (buildSiteSnapshot root) (removePathForcibly . fst) $ \(_, snapshotDir) -> do
        files <- relativeFiles snapshotDir
        defaultMain (goldenTests root snapshotDir files)
