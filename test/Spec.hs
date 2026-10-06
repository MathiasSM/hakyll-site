-- \| Runs the specs of every module (one MathiasSM.*Spec per library module)
import Control.Monad (unless)
import MathiasSM.CleanURLSpec qualified as CleanURL
import MathiasSM.ContentSpec qualified as Content
import MathiasSM.FeedsSpec qualified as Feeds
import MathiasSM.MetadataSpec qualified as Metadata
import MathiasSM.RedirectsSpec qualified as Redirects
import MathiasSM.TsvSpec qualified as Tsv
import MathiasSM.ValidateSpec qualified as Validate
import Support (group)
import System.Exit (exitFailure)
import Test.HUnit (Counts (..), runTestTT)

main :: IO ()
main = do
  Counts{errors, failures} <-
    runTestTT $ group "mathiassm" [CleanURL.tests, Content.tests, Feeds.tests, Metadata.tests, Redirects.tests, Tsv.tests, Validate.tests]
  unless (errors + failures == 0) exitFailure
