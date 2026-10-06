module MathiasSM.ValidateSpec (tests) where

import MathiasSM.Validate (missingHobbyPages)
import Support
import Test.HUnit

tests :: Test
tests =
  group
    "Validate"
    [ group
        "missingHobbyPages"
        [ it "is empty when every hobby has a post" $
            missingHobbyPages [hobby "/hobbies/drawing", hobby "/hobbies/tv"] ["/hobbies/tv", "/hobbies/drawing", "/blog/other"]
              ~?= []
        , it "lists the hobbies without a post, in table order" $
            missingHobbyPages [hobby "/hobbies/drawing", hobby "/hobbies/tv", hobby "/hobbies/chess"] ["/hobbies/tv"]
              ~?= ["/hobbies/drawing", "/hobbies/chess"]
        , it "ignores rows without an href" $
            missingHobbyPages [[("name", "Nothing")]] []
              ~?= []
        , it "counts hidden hobbies too" $
            missingHobbyPages [[("href", "/hobbies/secret"), ("show", "false")]] []
              ~?= ["/hobbies/secret"]
        ]
    ]
 where
  hobby href = [("name", "x"), ("href", href), ("show", "true")]
