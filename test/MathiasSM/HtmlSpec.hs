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
            [ testCase "makes a task-list checkbox read-only" $
                readonlyCheckboxes "<ul class=\"task-list\"><li><label><input type=\"checkbox\" /></label></li></ul>"
                    @?= "<ul class=\"task-list\"><li><label><input type=\"checkbox\" aria-readonly=\"true\" tabindex=\"-1\" /></label></li></ul>"
            , testCase "keeps a checked checkbox checked" $
                readonlyCheckboxes "<ul class=\"task-list\"><li><label><input type=\"checkbox\" checked=\"\" /></label></li></ul>"
                    @?= "<ul class=\"task-list\"><li><label><input type=\"checkbox\" checked aria-readonly=\"true\" tabindex=\"-1\" /></label></li></ul>"
            , testCase "leaves a checkbox outside a task list alone" $
                readonlyCheckboxes "<input type=\"checkbox\" />"
                    @?= "<input type=\"checkbox\" />"
            , testCase "leaves a checkbox in an interactive element editable" $
                readonlyCheckboxes "<div class=\"interactive\"><ul class=\"task-list\"><li><input type=\"checkbox\" /></li></ul></div>"
                    @?= "<div class=\"interactive\"><ul class=\"task-list\"><li><input type=\"checkbox\" /></li></ul></div>"
            , testCase "treats a nested interactive ancestor as interactive" $
                readonlyCheckboxes "<div class=\"interactive\"><div><ul class=\"task-list\"><input type=\"checkbox\" /></ul></div></div>"
                    @?= "<div class=\"interactive\"><div><ul class=\"task-list\"><input type=\"checkbox\" /></ul></div></div>"
            , testCase "treats a non-interactive ancestor as read-only" $
                readonlyCheckboxes "<div class=\"note\"><div><ul class=\"task-list\"><input type=\"checkbox\" /></ul></div></div>"
                    @?= "<div class=\"note\"><div><ul class=\"task-list\"><input type=\"checkbox\" aria-readonly=\"true\" tabindex=\"-1\" /></ul></div></div>"
            , testCase "only touches checkboxes, not other inputs" $
                readonlyCheckboxes "<input type=\"text\" /><ul class=\"task-list\"><input type=\"checkbox\" /></ul>"
                    @?= "<input type=\"text\" /><ul class=\"task-list\"><input type=\"checkbox\" aria-readonly=\"true\" tabindex=\"-1\" /></ul>"
            , testCase "leaves text without checkboxes untouched" $
                readonlyCheckboxes "<p>no <strong>boxes</strong></p>"
                    @?= "<p>no <strong>boxes</strong></p>"
            , testCase "is idempotent" $ do
                let once = readonlyCheckboxes "<ul class=\"task-list\"><input type=\"checkbox\" checked=\"\" /></ul>"
                readonlyCheckboxes once @?= once
            ]
        ]
