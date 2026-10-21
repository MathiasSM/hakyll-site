-- | Tests for the post-render HTML transforms
module MathiasSM.HtmlSpec (tests) where

import MathiasSM.Html (readonlyCheckboxes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | The HTML transform tests
tests :: TestTree
tests =
    testGroup
        "Html"
        [ testGroup
            "readonlyCheckboxes"
            [ testCase "makes a checkbox read-only" $
                readonlyCheckboxes "<input type=\"checkbox\" />"
                    @?= "<input type=\"checkbox\" aria-readonly=\"true\" tabindex=\"-1\" />"
            , testCase "keeps a checked checkbox checked" $
                readonlyCheckboxes "<input type=\"checkbox\" checked=\"\" />done"
                    @?= "<input type=\"checkbox\" checked aria-readonly=\"true\" tabindex=\"-1\" />done"
            , testCase "leaves a checkbox in an interactive element editable" $
                readonlyCheckboxes "<div class=\"interactive\"><input type=\"checkbox\" /></div>"
                    @?= "<div class=\"interactive\"><input type=\"checkbox\" /></div>"
            , testCase "treats a nested interactive ancestor as interactive" $
                readonlyCheckboxes "<div class=\"interactive\"><div><input type=\"checkbox\" /></div></div>"
                    @?= "<div class=\"interactive\"><div><input type=\"checkbox\" /></div></div>"
            , testCase "treats a non-interactive ancestor as read-only" $
                readonlyCheckboxes "<div class=\"note\"><div><input type=\"checkbox\" /></div></div>"
                    @?= "<div class=\"note\"><div><input type=\"checkbox\" aria-readonly=\"true\" tabindex=\"-1\" /></div></div>"
            , testCase "only touches checkboxes, not other inputs" $
                readonlyCheckboxes "<input type=\"text\" /><input type=\"checkbox\" />"
                    @?= "<input type=\"text\" /><input type=\"checkbox\" aria-readonly=\"true\" tabindex=\"-1\" />"
            , testCase "leaves text without checkboxes untouched" $
                readonlyCheckboxes "<p>no <strong>boxes</strong></p>"
                    @?= "<p>no <strong>boxes</strong></p>"
            , testCase "is idempotent" $ do
                let once = readonlyCheckboxes "<input type=\"checkbox\" checked=\"\" />x"
                readonlyCheckboxes once @?= once
            ]
        ]
