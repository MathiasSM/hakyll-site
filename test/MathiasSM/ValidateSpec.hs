-- | Tests for the build-time validations
module MathiasSM.ValidateSpec (tests) where

import MathiasSM.Validate (missingHobbyPages)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | The validation tests
tests :: TestTree
tests =
  testGroup
    "Validate"
    [ testGroup
        "missingHobbyPages"
        [ testCase "is empty when every hobby has a post" $
            missingHobbyPages [hobby "/hobbies/drawing", hobby "/hobbies/tv"] ["/hobbies/tv", "/hobbies/drawing", "/blog/other"]
              @?= [],
          testCase "lists the hobbies without a post, in table order" $
            missingHobbyPages [hobby "/hobbies/drawing", hobby "/hobbies/tv", hobby "/hobbies/chess"] ["/hobbies/tv"]
              @?= ["/hobbies/drawing", "/hobbies/chess"],
          testCase "ignores rows without an href" $
            missingHobbyPages [[("name", "Nothing")]] []
              @?= [],
          testCase "counts hidden hobbies too" $
            missingHobbyPages [[("href", "/hobbies/secret"), ("show", "false")]] []
              @?= ["/hobbies/secret"]
        ]
    ]
  where
    hobby href = [("name", "x"), ("href", href), ("show", "true")]
