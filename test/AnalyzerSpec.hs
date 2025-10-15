{-# HLINT ignore "Redundant do" #-}

module AnalyzerSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Property
import           Debug.Trace

import           AbsError                      as Err
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Lazy

import           Analyzer
import           AbsFlow
import           AbsData
import           Expr
import           Flow
import qualified Data.Map                      as M

participantStmt i n = Participant i n Nothing

spec :: Spec
spec = parallel $ do

  describe "tagStmt" $ do

    describe "Title" $ do

      let titleStmt = Title ParserInfo {row = 1, column = 1, src = ""} Text {t = ["untitled"]}

      context "when analysing a title statement" $ do

        it "top propagation should return an empty set" $ do
          let Expr { propagation = exprPropagation } = tagStmt titleStmt
          top exprPropagation [] `shouldBe` []

        it "bottom propagation should return an empty set" $ do
          let Expr { propagation = exprPropagation } = tagStmt titleStmt
          bottom exprPropagation [] `shouldBe` []

        it "expression should return the title with the passed set of entities" $ do
          let Expr { expr = titleExpr } = tagStmt titleStmt
          titleExpr [] [] [] `shouldBe` [Title (Info [] []) Text {t = ["untitled"]}]

    describe "Participant" $ do

      let participanStmt = Participant ParserInfo {row = 1, column = 1, src = ""} "A" Nothing

      context "when analysing a participan statement" $ do

        it "top propagation should return an empty set" $ do
          let Expr { propagation = exprPropagation } = tagStmt participanStmt
          top exprPropagation [] `shouldBe` [("A", On)]

        it "bottom propagation should return an empty set" $ do
          let Expr { propagation = exprPropagation } = tagStmt participanStmt
          bottom exprPropagation [] `shouldBe` [("A", On)]

        it "expression should return the participan with the passed set of entities" $ do
          let ex@Expr { expr = participantExpr } = tagStmt participanStmt
          participantExpr [] [] [] `shouldBe` [Participant (Info [("A", On)] [("A", On)]) "A" Nothing]

    describe "SetTo" $ do

      let setToStmt = SetTo ParserInfo {row = 1, column = 1, src = ""} "A" (StringValue "Test")

      context "when analysing a setTo statement" $ do

        it "top propagation should return an empty set" $ do
          let Expr { propagation = exprPropagation } = tagStmt setToStmt
          top exprPropagation [] `shouldBe` []

        it "bottom propagation should return an empty set" $ do
          let Expr { propagation = exprPropagation } = tagStmt setToStmt
          bottom exprPropagation [] `shouldBe` []

        it "expression should return the SetTo with the passed set of entities" $ do
          let ex@Expr { expr = setToxpr } = tagStmt setToStmt
          setToxpr [] [] [] `shouldBe` [SetTo (Info [] []) "A" (StringValue "Test")]

    describe "Header" $ do

      let setToStmt = Header ParserInfo {row = 1, column = 1, src = ""}
                             (Head "A" [AssignmentByValue (Identifier "v") (StringValue "Test")])

      context "when analysing a header statement" $ do

        it "top propagation should return an empty set" $ do
          let Expr { propagation = exprPropagation } = tagStmt setToStmt
          top exprPropagation [] `shouldBe` []

        it "bottom propagation should return an empty set" $ do
          let Expr { propagation = exprPropagation } = tagStmt setToStmt
          bottom exprPropagation [] `shouldBe` []

        it "expression should return the header with the passed set of entities" $ do
          let ex@Expr { expr = headerExpr } = tagStmt setToStmt
          headerExpr [] [] []
            `shouldBe` [Header (Info [] []) (Head "A" [AssignmentByValue (Identifier "v") (StringValue "Test")])]

    describe "Delay" $ do

      let setToStmt = Delay ParserInfo {row = 1, column = 1, src = ""} Text {t = ["untitled"]}

      context "when analysing a delay statement" $ do

        it "top propagation should return an empty set" $ do
          let Expr { propagation = exprPropagation, expr = groupExpr } = tagStmt setToStmt
          top exprPropagation [] `shouldBe` []

        it "bottom propagation should return an empty set" $ do
          let Expr { propagation = exprPropagation, expr = groupExpr } = tagStmt setToStmt
          bottom exprPropagation [] `shouldBe` []

        it "expression should return the delay with the passed set of entities" $ do
          let ex@Expr { expr = delayExpr } = tagStmt setToStmt
          delayExpr [] [] [] `shouldBe` [Delay (Info [] []) Text {t = ["untitled"]}]

    describe "Destroy" $ do

      let destroyStmt = Destroy ParserInfo {row = 1, column = 1, src = ""} "A"
      let entity      = [("A", On)]
      let offEntity   = [("A", Off)]

      context "when analysing a destroy statement" $ do

        it "top propagation should return passed entities without adding new ones" $ do
          let Expr { propagation = exprPropagation } = tagStmt destroyStmt
          top exprPropagation entity `shouldBe` entity

        it "bottom propagation should return all active entities but the removed one" $ do
          let Expr { propagation = exprPropagation } = tagStmt destroyStmt
          bottom exprPropagation entity `shouldBe` offEntity

        it "expression should return the destroy with the passed set of entities" $ do
          let ex@Expr { expr = destroyExpr } = tagStmt destroyStmt
          destroyExpr entity entity [] `shouldBe` [Destroy (Info entity offEntity) "A"]

    describe "Request" $ do

      let info = ParserInfo {row = 1, column = 1, src = ""}
          simpleReq e = RequestEntity e Nothing
          req i a b arrow = BasicRequest i a (arrow Thick) b Text {t = ["untitled"]} Nothing
          reqStmt a x y nest = Requests
            a
            (Request a
                     (req a (simpleReq x) (simpleReq y) NewBlockArrow)
                     nest
                     (Just (req a (simpleReq y) (simpleReq x) ClosingBlockArrow))
            )

      context "when analysing not nested request statement" $ do

        let entities  = [("A", On), ("B", On)]
        let singleReq = reqStmt info "A" "B" []

        it "top propagation should return all active entities" $ do
          let Expr { propagation = exprPropagation } = tagStmt singleReq
          top exprPropagation [] `shouldBe` entities

        it "bottom propagation should return all active entities but the removed one" $ do
          let Expr { propagation = exprPropagation } = tagStmt singleReq
          bottom exprPropagation [] `shouldBe` entities

        it "expression should return the request with the passed set of entities" $ do
          let ex@Expr { expr = reqExpr } = tagStmt singleReq
          reqExpr [] [] [] `shouldBe` [reqStmt (Info entities entities) "A" "B" []]

      context "when analysing new participant request" $ do

        let
          bottom_entities = [("A", On), ("B", On)]
          top_entities    = [("B", On)]
          reqNewParticipant a x y =
            Requests a (Request a (req a (simpleReq x) (simpleReq y) NewParticipantArrow) [] Nothing)
        let singleReq = reqNewParticipant info "A" "B"

        it "top propagation should return all active entities" $ do
          let Expr { propagation = exprPropagation } = tagStmt singleReq
          top exprPropagation [] `shouldBe` top_entities

        it "bottom propagation should return all active entities but the removed one" $ do
          let Expr { propagation = exprPropagation } = tagStmt singleReq
          bottom exprPropagation [] `shouldBe` bottom_entities

        it "expression should return the request with the passed set of entities" $ do
          let ex@Expr { expr = reqExpr } = tagStmt singleReq
          reqExpr [] [] [] `shouldBe` [reqNewParticipant (Info top_entities bottom_entities) "A" "B"]

      context "when analysing nested request statement" $ do

        let entities  = [("A", On), ("B", On), ("C", On), ("D", On)]
        let singleReq = reqStmt info "C" "D" []
        let nestReq   = reqStmt info "A" "B" [singleReq]

        it "top propagation should return all active entities" $ do
          let Expr { propagation = exprPropagation } = tagStmt nestReq
          top exprPropagation [] `shouldBe` entities

        it "bottom propagation should return all active entities but the removed one" $ do
          let Expr { propagation = exprPropagation } = tagStmt nestReq
          bottom exprPropagation [] `shouldBe` entities

        it "expression should return the request with the passed set of entities" $ do
          let ex@Expr { expr = reqExpr } = tagStmt nestReq
              info                       = Info entities entities
          reqExpr [] [] [] `shouldBe` [reqStmt info "A" "B" [reqStmt info "C" "D" []]]

    describe "Include" $ do

      let info        = ParserInfo {row = 1, column = 1, src = ""}
      let includeStmt = Include info "" M.empty

      context "when analysing empty include" $ do

        it "top propagation should return all active entities" $ do
          let Expr { propagation = exprPropagation } = tagStmt (includeStmt [])
          top exprPropagation [] `shouldBe` []

        it "bottom propagation should return all active entities but the removed one" $ do
          let Expr { propagation = exprPropagation } = tagStmt (includeStmt [])
          bottom exprPropagation [] `shouldBe` []

        it "expression should return the include stmt with the passed set of entities" $ do
          let ex@Expr { expr = includeExpr } = tagStmt (includeStmt [])
          includeExpr [] [] [] `shouldBe` []


      context "when analysing an include with stmts" $ do

        let expectedParticipants = [("A", On), ("B", On)]

        it "top propagation should return all active entities" $ do
          let Expr { propagation = exprPropagation } =
                tagStmt (includeStmt [participantStmt info "A", participantStmt info "B"])
          top exprPropagation [] `shouldBe` expectedParticipants

        it "bottom propagation should return all active entities but the removed one" $ do
          let Expr { propagation = exprPropagation } =
                tagStmt (includeStmt [participantStmt info "A", participantStmt info "B"])
          bottom exprPropagation [] `shouldBe` expectedParticipants

        it "expression should return the include stmt with the passed set of entities" $ do
          let ex@Expr { expr = includeExpr } = tagStmt (includeStmt [participantStmt info "A", participantStmt info "B"])
              infoTag                        = Info expectedParticipants expectedParticipants
          includeExpr [] [] [] `shouldBe` [participantStmt infoTag "A", participantStmt infoTag "B"]

    describe "Group" $ do

      let info = ParserInfo {row = 1, column = 1, src = ""}
      let groupStmt i = Group i Text {t = [""]}

      context "when analysing group with stmts" $ do

        let expectedParticipants = [("A", On), ("B", On)]

        it "top propagation should return all active entities" $ do
          let Expr { propagation = exprPropagation } =
                tagStmt (groupStmt info [participantStmt info "A", participantStmt info "B"])
          top exprPropagation [] `shouldBe` expectedParticipants

        it "bottom propagation should return all active entities but the removed one" $ do
          let Expr { propagation = exprPropagation } =
                tagStmt (groupStmt info [participantStmt info "A", participantStmt info "B"])
          bottom exprPropagation [] `shouldBe` expectedParticipants

        it "expression should return the request with the passed set of entities" $ do
          let ex@Expr { expr = groupExpr } = tagStmt (groupStmt info [participantStmt info "A", participantStmt info "B"])
              infoTag                      = Info expectedParticipants expectedParticipants
          groupExpr [] [] [] `shouldBe` [groupStmt infoTag [participantStmt infoTag "A", participantStmt infoTag "B"]]

    describe "LabelledGroup" $ do

      let info = ParserInfo {row = 1, column = 1, src = ""}
      let groupStmt i = LabelledGroup i Loop
      let labelingStmt i = Labelling i Text {t = [""]}

      context "when analysing labelled group with stmts" $ do

        let expectedParticipants = [("A", On), ("B", On)]

        it "top propagation should return all active entities" $ do
          let Expr { propagation = exprPropagation } =
                tagStmt (groupStmt info [labelingStmt info [participantStmt info "A", participantStmt info "B"]])
          top exprPropagation [] `shouldBe` expectedParticipants

        it "bottom propagation should return all active entities but the removed one" $ do
          let Expr { propagation = exprPropagation } =
                tagStmt (groupStmt info [labelingStmt info [participantStmt info "A", participantStmt info "B"]])
          bottom exprPropagation [] `shouldBe` expectedParticipants

        it "expression should return the request with the passed set of entities" $ do
          let ex@Expr { expr = labeldGroupExpr } =
                tagStmt (groupStmt info [labelingStmt info [participantStmt info "A", participantStmt info "B"]])
              infoTag = Info expectedParticipants expectedParticipants
          labeldGroupExpr [] [] []
            `shouldBe` [groupStmt infoTag [labelingStmt infoTag [participantStmt infoTag "A", participantStmt infoTag "B"]]]

    describe "Note" $ do

      let info = ParserInfo {row = 1, column = 1, src = ""}
      let noteStmt pos i es = Note i pos es Text {t = [""]}

      describe "Left" $ do

        let entities = ["A"]

        context "when analysing note stmt" $ do

          let expectedParticipants = [("A", On)]

          it "top propagation should return all active entities" $ do

            let Expr { propagation = exprPropagation } = tagStmt (noteStmt NoteLeft info entities)
            top exprPropagation [] `shouldBe` expectedParticipants

          it "bottom propagation should return all active entities but the removed one" $ do
            let Expr { propagation = exprPropagation } = tagStmt (noteStmt NoteLeft info entities)
            bottom exprPropagation [] `shouldBe` expectedParticipants

          it "expression should return the request with the passed set of entities" $ do
            let ex@Expr { expr = noteExpr } = tagStmt (noteStmt NoteLeft info entities)
            noteExpr [] [] [] `shouldBe` [noteStmt NoteLeft (Info expectedParticipants expectedParticipants) entities]

      describe "Right" $ do

        let entities = ["A"]

        context "when analysing note stmt" $ do

          let expectedParticipants = [("A", On)]

          it "top propagation should return all active entities" $ do

            let Expr { propagation = exprPropagation } = tagStmt (noteStmt NoteRight info entities)
            top exprPropagation [] `shouldBe` expectedParticipants

          it "bottom propagation should return all active entities but the removed one" $ do
            let Expr { propagation = exprPropagation } = tagStmt (noteStmt NoteRight info entities)
            bottom exprPropagation [] `shouldBe` expectedParticipants

          it "expression should return the request with the passed set of entities" $ do
            let ex@Expr { expr = noteExpr } = tagStmt (noteStmt NoteRight info entities)
            noteExpr [] [] [] `shouldBe` [noteStmt NoteRight (Info expectedParticipants expectedParticipants) entities]

      describe "Above" $ do

        let entities = ["A", "B"]

        context "when analysing note stmt" $ do

          let expectedParticipants = [("A", On), ("B", On)]

          it "top propagation should return all active entities" $ do

            let Expr { propagation = exprPropagation } = tagStmt (noteStmt NoteAbove info entities)
            top exprPropagation [] `shouldBe` expectedParticipants

          it "bottom propagation should return all active entities but the removed one" $ do
            let Expr { propagation = exprPropagation } = tagStmt (noteStmt NoteAbove info entities)
            bottom exprPropagation [] `shouldBe` expectedParticipants

          it "expression should return the request with the passed set of entities" $ do
            let ex@Expr { expr = noteExpr } = tagStmt (noteStmt NoteAbove info entities)
            noteExpr [] [] [] `shouldBe` [noteStmt NoteAbove (Info expectedParticipants expectedParticipants) entities]

  describe "tag" $ do

    describe "Title" $ do
      let dummyInfo = ParserInfo {row = 1, column = 1, src = ""}
          participant info entity = Participant info entity Nothing
          diagram          = DiagramRule dummyInfo [participant dummyInfo "A", participant dummyInfo "B"]
          expectedEntities = [("A", On), ("B", On)]
          expectedInfo     = Info expectedEntities expectedEntities

      context "when title stmts that generate entities are analysed" $ do

        it "expression should return the title with the passed set of entities" $ do
          tag diagram `shouldBe` DiagramRule expectedInfo [participant expectedInfo "A", participant expectedInfo "B"]

    describe "Request1" $ do
      let dummyInfo = ParserInfo {row = 1, column = 1, src = ""}
          participant info entity = Participant info entity Nothing
          simpleReq e = RequestEntity e Nothing
          req i a b arrow = BasicRequest i a (arrow Thick) b Text {t = ["untitled"]} Nothing
          reqStmt a x y nest = Requests
            a
            (Request a
                     (req a (simpleReq x) (simpleReq y) NewBlockArrow)
                     nest
                     (Just (req a (simpleReq y) (simpleReq x) ClosingBlockArrow))
            )

          diagram = DiagramRule
            dummyInfo
            [ participant dummyInfo "A"
            , participant dummyInfo "B"
            , reqStmt dummyInfo "D" "C" []
            , Destroy dummyInfo "D"
            , reqStmt dummyInfo "C" "A" []
            ]
          allOnEntities     = [("A", On), ("B", On), ("D", On), ("C", On)]
          allOnButDEntities = [("A", On), ("B", On), ("D", Off), ("C", On)]
          allOnInfo         = Info allOnEntities allOnEntities
          outDOffInfo       = Info allOnEntities allOnButDEntities
          allDOffInfo       = Info allOnButDEntities allOnButDEntities

      context "when request stmts that generate entities are analysed" $ do

        it "expression should return the request with the passed set of entities" $ do
          tag diagram `shouldBe` DiagramRule
            outDOffInfo
            [ participant allOnInfo "A"
            , participant allOnInfo "B"
            , reqStmt allOnInfo "D" "C" []
            , Destroy outDOffInfo "D"
            , reqStmt allDOffInfo "C" "A" []
            ]
