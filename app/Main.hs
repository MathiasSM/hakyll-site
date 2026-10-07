import Control.Monad (unless)
import Hakyll (hakyllWith)
import MathiasSM.Config (siteConfiguration)
import MathiasSM.Site (rules)
import MathiasSM.Validate (ensureContentExists)
import System.Environment (getArgs)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  -- `clean` only removes generated files, so it works without the content
  unless ("clean" `elem` args) ensureContentExists
  hakyllWith siteConfiguration rules
