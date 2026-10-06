-- | Runs the specs of every module (one MathiasSM.*Spec per library module)
import Control.Monad (unless)
import qualified MathiasSM.CleanURLSpec as CleanURL
import qualified MathiasSM.ContentSpec as Content
import qualified MathiasSM.FeedsSpec as Feeds
import qualified MathiasSM.MetadataSpec as Metadata
import qualified MathiasSM.RedirectsSpec as Redirects
import qualified MathiasSM.TsvSpec as Tsv
import qualified MathiasSM.ValidateSpec as Validate
import Support (group)
import System.Exit (exitFailure)
import Test.HUnit (Counts (..), runTestTT)

main :: IO ()
main = do
  Counts{errors, failures} <-
    runTestTT $ group "mathiassm" [CleanURL.tests, Content.tests, Feeds.tests, Metadata.tests, Redirects.tests, Tsv.tests, Validate.tests]
  unless (errors + failures == 0) exitFailure
