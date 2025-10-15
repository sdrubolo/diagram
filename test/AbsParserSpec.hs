{-# HLINT ignore "Redundant do" #-}

module AbsParserSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Property



import           AbsParser
import           AbsFlow
import           AbsData
import           Debug.Trace

spec :: Spec
spec = parallel $ do

  describe "Parse Program" $ do

    describe "Empty Diagram" $ do

      context "when parsing an empty diagram" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "" Nothing
          parseTree `shouldBe` DiagramRule ParserInfo {row = 1, column = 1, src = ""} []

    describe "Note" $ do

      describe "Over" $ do

        context "when parsing note with more entities" $ do

          it "should return the proper abstract syntax" $ do
            Right parseTree <- parseString "note over test, test_1, test_2 \n left \n end note" Nothing
            parseTree `shouldBe` DiagramRule
              ParserInfo {row = 1, column = 1, src = ""}
              [Note ParserInfo {row = 1, column = 1, src = ""} NoteAbove ["test", "test_1", "test_2"] Text {t = ["left"]}]

        context "when parsing note with single entity" $ do

          it "should return the proper abstract syntax" $ do
            Right parseTree <- parseString "note over d \n left \n end note" Nothing
            parseTree `shouldBe` DiagramRule
              ParserInfo {row = 1, column = 1, src = ""}
              [Note ParserInfo {row = 1, column = 1, src = ""} NoteAbove ["d"] Text {t = ["left"]}]

        context "when parsing note with colon" $ do

          it "should return the proper abstract syntax" $ do
            Right parseTree <- parseString "note over d : left\n" Nothing
            parseTree `shouldBe` DiagramRule
              ParserInfo {row = 1, column = 1, src = ""}
              [Note ParserInfo {row = 1, column = 1, src = ""} NoteAbove ["d"] Text {t = ["left"]}]

      describe "Left" $ do

        context "when parsing note" $ do

          it "should return the proper abstract syntax" $ do
            Right parseTree <- parseString "note left of d \n left \n end note" Nothing
            parseTree `shouldBe` DiagramRule
              ParserInfo {row = 1, column = 1, src = ""}
              [Note ParserInfo {row = 1, column = 1, src = ""} NoteLeft ["d"] Text {t = ["left"]}]

        context "when parsing note with colon" $ do

          it "should return the proper abstract syntax" $ do
            Right parseTree <- parseString "note left of d : left\n" Nothing
            parseTree `shouldBe` DiagramRule
              ParserInfo {row = 1, column = 1, src = ""}
              [Note ParserInfo {row = 1, column = 1, src = ""} NoteLeft ["d"] Text {t = ["left"]}]

      describe "Right" $ do

        context "when parsing note" $ do

          it "should return the proper abstract syntax" $ do
            Right parseTree <- parseString "note right of d \n left \n end note" Nothing
            parseTree `shouldBe` DiagramRule
              ParserInfo {row = 1, column = 1, src = ""}
              [Note ParserInfo {row = 1, column = 1, src = ""} NoteRight ["d"] Text {t = ["left"]}]

        context "when parsing note with colon" $ do

          it "should return the proper abstract syntax" $ do
            Right parseTree <- parseString "note right of d : left\n" Nothing
            parseTree `shouldBe` DiagramRule
              ParserInfo {row = 1, column = 1, src = ""}
              [Note ParserInfo {row = 1, column = 1, src = ""} NoteRight ["d"] Text {t = ["left"]}]

    describe "Group" $ do

      context "when parsing an empty group" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "group : \n end group" Nothing
          parseTree `shouldBe` DiagramRule ParserInfo {row = 1, column = 1, src = ""}
                                           [Group ParserInfo {row = 1, column = 1, src = ""} Text {t = []} []]

        context "when parsing an empty group with title" $ do

          it "should return the proper abstract syntax" $ do
            Right parseTree <- parseString "group : test \n end group" Nothing
            parseTree `shouldBe` DiagramRule ParserInfo {row = 1, column = 1, src = ""}
                                             [Group ParserInfo {row = 1, column = 1, src = ""} Text {t = ["test"]} []]

    describe "Delay" $ do
      context "when parsing" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "... this is a. cane test ..." Nothing
          parseTree `shouldBe` DiagramRule
            ParserInfo {row = 1, column = 1, src = ""}
            [Delay ParserInfo {row = 1, column = 1, src = ""} Text {t = ["this is a. cane test"]}]

    describe "Title" $ do
      context "when on a single row" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "title Untitled\n" Nothing
          parseTree `shouldBe` DiagramRule ParserInfo {row = 1, column = 1, src = ""}
                                           [Title ParserInfo {row = 1, column = 1, src = ""} Text {t = ["Untitled"]}]

      context "when on multiple rows" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "title Untitled \\n New Row\n" Nothing
          parseTree `shouldBe` DiagramRule
            ParserInfo {row = 1, column = 1, src = ""}
            [Title ParserInfo {row = 1, column = 1, src = ""} Text {t = ["Untitled ", " New Row"]}]

    describe "Destroy" $ do
      context "when parsing" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "destroy Entity\n" Nothing
          parseTree `shouldBe` DiagramRule ParserInfo {row = 1, column = 1, src = ""}
                                           [Destroy ParserInfo {row = 1, column = 1, src = ""} "Entity"]

    describe "Value" $ do
      context "when value is a string" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "set Entity\n to \"test\"" Nothing
          parseTree `shouldBe` DiagramRule ParserInfo {row = 1, column = 1, src = ""}
                                           [SetTo ParserInfo {row = 1, column = 1, src = ""} "Entity" (StringValue "test")]

      context "when value is a boolean" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "set Entity\n to true" Nothing
          parseTree `shouldBe` DiagramRule ParserInfo {row = 1, column = 1, src = ""}
                                           [SetTo ParserInfo {row = 1, column = 1, src = ""} "Entity" (BoolValue True)]

      context "when value is an integer" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "set Entity\n to 1" Nothing
          parseTree `shouldBe` DiagramRule ParserInfo {row = 1, column = 1, src = ""}
                                           [SetTo ParserInfo {row = 1, column = 1, src = ""} "Entity" (IntegerValue 1)]

    describe "Header" $ do
      context "when value is a string" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "header for Entity\n data test=\"test\"; end data" Nothing
          parseTree `shouldBe` DiagramRule
            ParserInfo {row = 1, column = 1, src = ""}
            [ Header ParserInfo {row = 1, column = 1, src = ""}
                     (Head "Entity" [AssignmentByValue (Identifier "test") (StringValue "test")])
            ]


      context "when value is a boolean" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "header for Entity\n data test=true; end data" Nothing
          parseTree `shouldBe` DiagramRule
            ParserInfo {row = 1, column = 1, src = ""}
            [ Header ParserInfo {row = 1, column = 1, src = ""}
                     (Head "Entity" [AssignmentByValue (Identifier "test") (BoolValue True)])
            ]

      context "when value is an integer" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "header for Entity\n data test=1; end data" Nothing
          parseTree `shouldBe` DiagramRule
            ParserInfo {row = 1, column = 1, src = ""}
            [ Header ParserInfo {row = 1, column = 1, src = ""}
                     (Head "Entity" [AssignmentByValue (Identifier "test") (IntegerValue 1)])
            ]

    describe "Participant" $ do
      context "when parsing participant without path" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "participant Entity\n" Nothing
          parseTree `shouldBe` DiagramRule ParserInfo {row = 1, column = 1, src = ""}
                                           [Participant ParserInfo {row = 1, column = 1, src = ""} "Entity" Nothing]

      context "when parsing participant path" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "participant Entity[pathToResource]" Nothing
          parseTree `shouldBe` DiagramRule
            ParserInfo {row = 1, column = 1, src = ""}
            [Participant ParserInfo {row = 1, column = 1, src = ""} "Entity" (Just "pathToResource")]

    describe "Comment" $ do
      context "when parsing" $ do

        it "should return the proper abstract syntax" $ do
          Right parseTree <- parseString "# this is a comment\n" Nothing
          parseTree `shouldBe` DiagramRule ParserInfo {row = 1, column = 1, src = ""} []

    describe "Request" $ do

      describe "Single request" $ do

        let info = ParserInfo {row = 1, column = 1, src = ""}
            request arrow =
              BasicRequest info (RequestEntity "A" Nothing) (arrow Thick) (RequestEntity "B" Nothing) Text {t = ["none"]}
            diagram req = DiagramRule info [Requests info (Request info req [] Nothing)]

        describe "when normal request" $ do

          context "when with no data" $ do

            it "should return the proper abstract syntax" $ do
              Right parseTree <- parseString "A -> B : none\n" Nothing
              parseTree `shouldBe` diagram (request Arrow Nothing)

          context "when with json data" $ do

            it "should return the proper abstract syntax" $ do
              Right parseTree <- parseString "A -> B : none\n data application/json { \"key\" : 1 } end data" Nothing
              parseTree `shouldBe` diagram (request Arrow (Just $ Json (JsonObject [("key", JsonInteger 1)])))

          context "when with encoded form data" $ do

            it "should return the proper abstract syntax" $ do
              Right parseTree <- parseString "A -> B : none\n data application/x-www-form-urlencoded key=1; end data" Nothing
              parseTree
                `shouldBe` diagram (request Arrow (Just $ Form [AssignmentByValue (Identifier "key") (IntegerValue 1)]))

        describe "when new participant request" $ do

          context "when with no data" $ do

            it "should return the proper abstract syntax" $ do
              Right parseTree <- parseString "A ->* B : none\n" Nothing
              parseTree `shouldBe` diagram (request NewParticipantArrow Nothing)

        describe "when failed request" $ do

          context "when with no data" $ do

            it "should return the proper abstract syntax" $ do
              Right parseTree <- parseString "A -X B : none\n" Nothing
              parseTree `shouldBe` diagram (request FailedArrow Nothing)

      describe "Block request" $ do

        let infoA = ParserInfo {row = 1, column = 1, src = ""}
            infoB = ParserInfo {row = 2, column = 1, src = ""}
            infoC = ParserInfo {row = 3, column = 1, src = ""}
            request a b arrow info =
              BasicRequest info (RequestEntity a Nothing) (arrow Thick) (RequestEntity b Nothing) Text {t = ["none"]}
            diagram reqA reqB = DiagramRule infoA [Requests infoA (Request infoA reqA [] (Just reqB))]

        describe "when normal request" $ do

          context "when with no data" $ do

            it "should return the proper abstract syntax" $ do
              Right parseTree <- parseString "A ->+ B : none\nB ->- A : none\n" Nothing
              let rA = request "A" "B" NewBlockArrow infoA Nothing
                  rB = request "B" "A" ClosingBlockArrow infoB Nothing
              parseTree `shouldBe` diagram rA rB

          context "when with json data" $ do

            it "should return the proper abstract syntax" $ do
              Right parseTree <- parseString
                "A ->+ B : none\n data application/json { \"key\" : 1 } end data\nB ->- A : none\n"
                Nothing
              let jsonData = Just $ Json (JsonObject [("key", JsonInteger 1)])
                  rA       = request "A" "B" NewBlockArrow infoA jsonData
                  rB       = request "B" "A" ClosingBlockArrow infoC Nothing
              parseTree `shouldBe` diagram rA rB

          context "when with encoded form data" $ do

            it "should return the proper abstract syntax" $ do
              Right parseTree <- parseString
                "A ->+ B : none\n data application/x-www-form-urlencoded key=1; end data\nB ->- A : none\n"
                Nothing
              let formData = Just $ Form [AssignmentByValue (Identifier "key") (IntegerValue 1)]
                  rA       = request "A" "B" NewBlockArrow infoA formData
                  rB       = request "B" "A" ClosingBlockArrow infoC Nothing
              parseTree `shouldBe` diagram rA rB


        describe "when failed request" $ do

          context "when with no data" $ do

            it "should return the proper abstract syntax" $ do
              Right parseTree <- parseString "A ->+ B : none\nB -X- A : none\n" Nothing
              let rA = request "A" "B" NewBlockArrow infoA Nothing
                  rB = request "B" "A" FailedArrow infoB Nothing
              parseTree `shouldBe` diagram rA rB

