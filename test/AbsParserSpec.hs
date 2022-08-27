{-# HLINT ignore "Redundant do" #-}

module AbsParserSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Property
import           Debug.Trace



import           AbsParser
import           AbsFlow

spec :: Spec
spec = do

  describe "Parse" $ do

    describe "Empty Diagram" $ do

      context "when parsing an empty diagram" $ do

        it "should return the proper abstract syntax" $ do
          Right (_, parseTree) <- parseString "" Nothing
          parseTree `shouldBe` (DiagramRule Info {row = 1, column = 1, src = ""} [])

      context "when parsing note over with single entity" $ do

        it "should return the proper abstract syntax" $ do
          Right (_, parseTree) <- parseString "note over d \n left \n end note" Nothing
          parseTree
            `shouldBe` (DiagramRule Info {row = 1, column = 1, src = ""}
                                    [Note Info {row = 1, column = 1, src = ""} NoteAbove ["d"] Text {t = ["left"]}]
                       )

    describe "Note" $ do

      describe "Over" $ do

        context "when parsing note with more entities" $ do

          it "should return the proper abstract syntax" $ do
            Right (_, parseTree) <- parseString "note over test, test_1, test_2 \n left \n end note " Nothing
            parseTree
              `shouldBe` (DiagramRule
                           Info {row = 1, column = 1, src = ""}
                           [ Note Info {row = 1, column = 1, src = ""}
                                  NoteAbove
                                  ["test", "test_1", "test_2"]
                                  Text {t = ["left"]}
                           ]
                         )

        context "when parsing note with single entity" $ do

          it "should return the proper abstract syntax" $ do
            Right (_, parseTree) <- parseString "note over d \n left \n end note" Nothing
            parseTree
              `shouldBe` (DiagramRule Info {row = 1, column = 1, src = ""}
                                      [Note Info {row = 1, column = 1, src = ""} NoteAbove ["d"] Text {t = ["left"]}]
                         )

        context "when parsing note with colon" $ do

          it "should return the proper abstract syntax" $ do
            Right (_, parseTree) <- parseString "note over d : left\n" Nothing
            parseTree
              `shouldBe` (DiagramRule Info {row = 1, column = 1, src = ""}
                                      [Note Info {row = 1, column = 1, src = ""} NoteAbove ["d"] Text {t = ["left"]}]
                         )

      describe "Left" $ do

        context "when parsing note" $ do

          it "should return the proper abstract syntax" $ do
            Right (_, parseTree) <- parseString "note left of d \n left \n end note" Nothing
            parseTree
              `shouldBe` (DiagramRule Info {row = 1, column = 1, src = ""}
                                      [Note Info {row = 1, column = 1, src = ""} NoteLeft ["d"] Text {t = ["left"]}]
                         )

        context "when parsing note with colon" $ do

          it "should return the proper abstract syntax" $ do
            Right (_, parseTree) <- parseString "note left of d : left\n" Nothing
            parseTree
              `shouldBe` (DiagramRule Info {row = 1, column = 1, src = ""}
                                      [Note Info {row = 1, column = 1, src = ""} NoteLeft ["d"] Text {t = ["left"]}]
                         )

      describe "Right" $ do

        context "when parsing note" $ do

          it "should return the proper abstract syntax" $ do
            Right (_, parseTree) <- parseString "note right of d \n left \n end note" Nothing
            parseTree
              `shouldBe` (DiagramRule Info {row = 1, column = 1, src = ""}
                                      [Note Info {row = 1, column = 1, src = ""} NoteRigth ["d"] Text {t = ["left"]}]
                         )

        context "when parsing note with colon" $ do

          it "should return the proper abstract syntax" $ do
            Right (_, parseTree) <- parseString "note right of d : left\n" Nothing
            parseTree
              `shouldBe` (DiagramRule Info {row = 1, column = 1, src = ""}
                                      [Note Info {row = 1, column = 1, src = ""} NoteRigth ["d"] Text {t = ["left"]}]
                         )

    describe "Group" $ do

      context "when parsing an empty group" $ do

        it "should return the proper abstract syntax" $ do
          Right (_, parseTree) <- parseString "group : \n end group" Nothing
          parseTree
            `shouldBe` (DiagramRule Info {row = 1, column = 1, src = ""}
                                    [Group Info {row = 1, column = 1, src = ""} Text {t = []} []]
                       )

        context "when parsing an empty group with title" $ do

          it "should return the proper abstract syntax" $ do
            Right (_, parseTree) <- parseString "group : test \n end group" Nothing
            parseTree
              `shouldBe` (DiagramRule Info {row = 1, column = 1, src = ""}
                                      [Group Info {row = 1, column = 1, src = ""} Text {t = ["test"]} []]
                         )
