-- | Tests for the build-time validations
module MathiasSM.ValidateSpec (tests) where

import MathiasSM.Validate (missingHobbyPages)
import MathiasSM.Yaml.Hobby (Hobby (..))
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
                    @?= []
            , testCase "lists the hobbies without a post, in file order" $
                missingHobbyPages [hobby "/hobbies/drawing", hobby "/hobbies/tv", hobby "/hobbies/chess"] ["/hobbies/tv"]
                    @?= ["/hobbies/drawing", "/hobbies/chess"]
            , testCase "counts hidden hobbies too" $
                missingHobbyPages [hobby "/hobbies/secret"] []
                    @?= ["/hobbies/secret"]
            , testCase "is empty when there are no hobbies" $
                missingHobbyPages [] [] @?= []
            ]
        ]
  where
    hobby href = Hobby "x" href "icon" True
