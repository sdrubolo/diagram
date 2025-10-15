{-# HLINT ignore "Redundant do" #-}

module EnvSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Property
import           Debug.Trace


import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           Env
import           Font
import qualified Data.Vector                   as V
import qualified Data.Map                      as Map
import qualified Data.Set                      as S
import           TTF

fontTable :: FontTable TTF
fontTable = FontTable
  { name        = "FontName"
  , lineHeigth  = 10
  , src         = ""
  , fontSize    = 10
  , fontLineGap = 2
  , table       = Map.fromList
    [ ('a', Size {_width = 10, yMin = 20, yMax = 40})
    , ('d', Size {_width = 10, yMin = 20, yMax = 40})
    , ('e', Size {_width = 10, yMin = 20, yMax = 40})
    , ('i', Size {_width = 10, yMin = 20, yMax = 40})
    , ('l', Size {_width = 10, yMin = 20, yMax = 40})
    , ('n', Size {_width = 10, yMin = 20, yMax = 40})
    , ('o', Size {_width = 10, yMin = 20, yMax = 40})
    , ('r', Size {_width = 10, yMin = 20, yMax = 40})
    , ('t', Size {_width = 10, yMin = 20, yMax = 40})
    , ('u', Size {_width = 10, yMin = 20, yMax = 40})
    , ('w', Size {_width = 10, yMin = 20, yMax = 40})
    ]
  , fontType    = "fontType"
  }


spec :: Spec
spec = parallel $ do

  describe "Env utilities functions" $ do

    describe "involvedEntities" $ do

      context "when a list containing the extreme elements in the list in order" $ do

        it "should return a sorted list of all entities" $ do
          let env = Env
                fontTable
                EntityData
                  { all_entities    = V.fromList ["A", "B", "C", "D"]
                  , entity_to_index = Map.fromList [("A", 0), ("B", 1), ("C", 2), ("D", 3)]
                  , scope           = S.empty
                  }
              Right entiries = evalState (runExceptT (involvedEntities ["A", "D"])) env
          V.toList entiries `shouldBe` ["A", "B", "C", "D"]


      context "when a list containing duplicated elements in the list" $ do

        it "should ignore them and return a sorted list of all entities" $ do
          let env = Env
                fontTable
                EntityData
                  { all_entities    = V.fromList ["A", "B", "C", "D"]
                  , entity_to_index = Map.fromList [("A", 0), ("B", 1), ("C", 2), ("D", 3)]
                  , scope           = S.empty
                  }
              Right entiries = evalState (runExceptT (involvedEntities ["A", "D", "A"])) env
          V.toList entiries `shouldBe` ["A", "B", "C", "D"]

      context "when a list containing the extreme elements in the list in reversed order" $ do

        it "should return a sorted list of all entities" $ do
          let env = Env
                fontTable
                EntityData
                  { all_entities    = V.fromList ["A", "B", "C", "D"]
                  , entity_to_index = Map.fromList [("A", 0), ("B", 1), ("C", 2), ("D", 3)]
                  , scope           = S.empty
                  }
              Right entiries = evalState (runExceptT (involvedEntities ["D", "A"])) env
          V.toList entiries `shouldBe` ["A", "B", "C", "D"]

      context "when a list containing the inner elements in the list in order" $ do

        it "should return a sorted list of entities" $ do
          let env = Env
                fontTable
                EntityData
                  { all_entities    = V.fromList ["A", "B", "C", "D", "E"]
                  , entity_to_index = Map.fromList [("A", 0), ("B", 1), ("C", 2), ("D", 3), ("E", 4)]
                  , scope           = S.empty
                  }
              Right entiries = evalState (runExceptT (involvedEntities ["B", "D"])) env
          V.toList entiries `shouldBe` ["B", "C", "D"]

      context "when a list containing the inner elements in the list in reversed order" $ do

        it "should return a sorted list of entities" $ do
          let env = Env
                fontTable
                EntityData
                  { all_entities    = V.fromList ["A", "B", "C", "D", "E"]
                  , entity_to_index = Map.fromList [("A", 0), ("B", 1), ("C", 2), ("D", 3), ("E", 4)]
                  , scope           = S.empty
                  }
              Right entiries = evalState (runExceptT (involvedEntities ["D", "B"])) env
          V.toList entiries `shouldBe` ["B", "C", "D"]

      context "when a list containing only one element" $ do

        it "should return a list with one element" $ do
          let env = Env
                fontTable
                EntityData
                  { all_entities    = V.fromList ["A", "B", "C", "D", "E"]
                  , entity_to_index = Map.fromList [("A", 0), ("B", 1), ("C", 2), ("D", 3), ("E", 4)]
                  , scope           = S.empty
                  }
              Right entiries = evalState (runExceptT (involvedEntities ["E"])) env
          V.toList entiries `shouldBe` ["E"]

      context "when a list containing only a no existing entity" $ do

        it "should return a sorted list of entities" $ do
          let env = Env
                fontTable
                EntityData
                  { all_entities    = V.fromList ["A", "B", "C", "D", "E"]
                  , entity_to_index = Map.fromList [("A", 0), ("B", 1), ("C", 2), ("D", 3), ("E", 4)]
                  , scope           = S.empty
                  }
              Right entiries = evalState (runExceptT (involvedEntities ["B", "H", "C"])) env
          V.toList entiries `shouldBe` ["B", "C"]

      context "when a list containing only a no existing entity" $ do

        it "should return an empty list of entities" $ do
          let env = Env
                fontTable
                EntityData
                  { all_entities    = V.fromList ["A", "B", "C", "D", "E"]
                  , entity_to_index = Map.fromList [("A", 0), ("B", 1), ("C", 2), ("D", 3), ("E", 4)]
                  , scope           = S.empty
                  }
              Right entiries = evalState (runExceptT (involvedEntities ["H"])) env
          V.toList entiries `shouldBe` []

    describe "nextEntity" $ do

      let env = Env
            fontTable
            EntityData
              { all_entities    = V.fromList ["A", "B", "C", "D"]
              , entity_to_index = Map.fromList [("A", 0), ("B", 1), ("C", 2), ("D", 3)]
              , scope           = S.empty
              }

      context "when an existing entity with next is used" $ do

        it "should return the next entity" $ do
          let Right next = evalState (runExceptT (nextEntity "A")) env
          next `shouldBe` Just "B"

      context "when an existing entity without next is used" $ do

        it "should return Nothing" $ do
          let Right next = evalState (runExceptT (nextEntity "D")) env
          next `shouldBe` Nothing

      context "when a no existing entity is used" $ do

        it "should return Nothing" $ do
          let Right next = evalState (runExceptT (nextEntity "H")) env
          next `shouldBe` Nothing

    describe "prevEntity" $ do

      let env = Env
            fontTable
            EntityData
              { all_entities    = V.fromList ["A", "B", "C", "D"]
              , entity_to_index = Map.fromList [("A", 0), ("B", 1), ("C", 2), ("D", 3)]
              , scope           = S.empty
              }

      context "when an existing entity with prev is used" $ do

        it "should return the prev entity" $ do
          let Right next = evalState (runExceptT (prevEntity "D")) env
          next `shouldBe` Just "C"

      context "when an existing entity without prev is used" $ do

        it "should return Nothing" $ do
          let Right next = evalState (runExceptT (prevEntity "H")) env
          next `shouldBe` Nothing

      context "when a no existing entity is used" $ do

        it "should return Nothing" $ do
          let Right next = evalState (runExceptT (prevEntity "A")) env
          next `shouldBe` Nothing
