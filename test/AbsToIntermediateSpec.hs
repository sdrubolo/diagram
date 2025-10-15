{-# HLINT ignore "Redundant do" #-}

module AbsToIntermediateSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Property
import           Debug.Trace



import           AbsToIntermediate
import           Font
import           AbsFlow
import           AbsData
import qualified Data.Map                      as M
import           TTF
import qualified Data.Vector                   as V
import           Env
import           IntermediateInfo
import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           Env                            ( Env(entities) )
import qualified Data.Set                      as S
import           AbsToIntermediate              ( NotePosition(AboveNote) )

fontTable :: FontTable TTF
fontTable = FontTable
  { name        = "FontName"
  , lineHeigth  = 10
  , src         = ""
  , fontSize    = 10
  , fontLineGap = 2
  , table       = M.fromList
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
    , (' ', Size {_width = 10, yMin = 20, yMax = 40})
    , ('A', Size {_width = 10, yMin = 20, yMax = 40})
    , ('B', Size {_width = 10, yMin = 20, yMax = 40})
    , ('C', Size {_width = 10, yMin = 20, yMax = 40})
    , ('D', Size {_width = 10, yMin = 20, yMax = 40})
    ]
  , fontType    = "fontType"
  }

type EntityPositions = M.Map String Float

runWithEnv :: Font a => EntityData -> FontTable a -> EntityResume -> [Flow b] -> Either String (IntermediateInfo, [Stmt])
runWithEnv entities font info flowExp = evalState (runExceptT (flowsToIntermediate info flowExp)) env
  where env = Env font entities


run :: Font a => FontTable a -> EntityResume -> [Flow b] -> Either String (IntermediateInfo, [Stmt])
run = runWithEnv empty

buildEntities :: [String] -> EntityData
buildEntities [] = empty
buildEntities entities =
  let (_, all, index_map, entities_scope) = foldl acc (0, V.empty, M.empty, S.empty) entities
  in  EntityData all index_map entities_scope
 where
  acc (index, all, index_map, entities_scope) x =
    (index + 1, V.snoc all x, M.insert x index index_map, S.insert x entities_scope)

mkTxt character = Txt {max_width = 10, txt_info = [TxtInfo {txt_width = 10, txt_height = 52, txt = character}]}

twoRowsTxt = Txt
  { max_width = 80
  , txt_info  = [ TxtInfo {txt_width = 80, txt_height = 52, txt = "untitled"}
                , TxtInfo {txt_width = 70, txt_height = 52, txt = "new row"}
                ]
  }

buildInfo :: [(String, Float)] -> EntityResume
buildInfo l = EntityResume (M.fromList l)

spec :: Spec
spec = parallel $ do

  describe "Intermediate" $ do

    describe "Title" $ do

      context "when parsing a diagram with title on single row" $ do

        it "should return a title statement" $ do
          let Right stms =
                run fontTable (buildInfo mempty) [Title ParserInfo {row = 1, column = 1, src = ""} Text {t = ["untitled"]}]
          stms
            `shouldBe` ( mempty
                       , [ TitleStmt
                             (Txt {max_width = 80, txt_info = [TxtInfo {txt_width = 80, txt_height = 52, txt = "untitled"}]})
                         ]
                       )


      context "when parsing a diagram with title on multiple rows" $ do

        it "should return a title statement" $ do
          let Right stms = run fontTable
                               (buildInfo mempty)
                               [Title ParserInfo {row = 1, column = 1, src = ""} Text {t = ["untitled", "new row"]}]

          stms `shouldBe` (mempty, [TitleStmt twoRowsTxt])

    describe "Note" $ do

      describe "Left" $ do

        context "when parsing note with participant" $ do

          it "should return a note stmt" $ do
            let Right stms = run
                  fontTable
                  (buildInfo mempty)
                  [Note ParserInfo {row = 1, column = 1, src = ""} NoteLeft ["A"] Text {t = ["untitled", "new row"]}]

            stms
              `shouldBe` ( IntermediateInfo
                           { entity_info = M.fromList [("A", EntityInfo {width = (80, 5)})]
                           , entity_txt  = M.fromList [("A", mkTxt "A")]
                           }
                         , [NoteStmt (LeftNote "A") twoRowsTxt (V.fromList ["A"])]
                         )

      describe "Right" $ do

        context "when parsing note with participant" $ do

          it "should return a note stmt" $ do
            let Right stms = run
                  fontTable
                  (buildInfo mempty)
                  [Note ParserInfo {row = 1, column = 1, src = ""} NoteRight ["A"] Text {t = ["untitled", "new row"]}]

            stms
              `shouldBe` ( IntermediateInfo
                           { entity_info = M.fromList [("A", EntityInfo {width = (5, 80)})]
                           , entity_txt  = M.fromList [("A", mkTxt "A")]
                           }
                         , [NoteStmt (RightNote "A") twoRowsTxt (V.fromList ["A"])]
                         )

      describe "Above" $ do

        context "when parsing note with only one participant" $ do

          it "should return a note stmt" $ do
            let Right stms = run
                  fontTable
                  (buildInfo mempty)
                  [Note ParserInfo {row = 1, column = 1, src = ""} NoteAbove ["A"] Text {t = ["untitled", "new row"]}]

            stms
              `shouldBe` ( IntermediateInfo
                           { entity_info = M.fromList [("A", EntityInfo {width = (40, 40)})]
                           , entity_txt  = M.fromList [("A", mkTxt "A")]
                           }
                         , [NoteStmt (AboveNote "A" "A") twoRowsTxt (V.fromList ["A"])]
                         )

        context "when parsing note with more participants" $ do

          it "should return a note stmt" $ do

            let Right stms = run
                  fontTable
                  (buildInfo mempty)
                  [ Note ParserInfo {row = 1, column = 1, src = ""}
                         NoteAbove
                         ["A", "B", "C", "D"]
                         Text {t = ["untitled", "new row"]}
                  ]

            stms
              `shouldBe` ( IntermediateInfo
                           { entity_info = M.fromList
                             [ ("A", EntityInfo {width = (5, 26.666666)})
                             , ("B", EntityInfo {width = (26.666666, 26.666666)})
                             , ("C", EntityInfo {width = (26.666666, 26.666666)})
                             , ("D", EntityInfo {width = (26.666666, 5)})
                             ]
                           , entity_txt = M.fromList [("A", mkTxt "A"), ("B", mkTxt "B"), ("C", mkTxt "C"), ("D", mkTxt "D")]
                           }
                         , [NoteStmt (AboveNote "A" "D") twoRowsTxt (V.fromList ["A", "B", "C", "D"])]
                         )

    describe "Group" $ do

      context "when parsing an empty group" $ do

        it "should range over all entities and return an empty stmt list" $ do
          let info       = buildInfo [("A", 5), ("B", 31.666666), ("C", 58.333332), ("D", 85)]
              Right stms = runWithEnv
                (buildEntities ["A", "B", "C", "D"])
                fontTable
                info
                [Group ParserInfo {row = 1, column = 1, src = ""} Text {t = ["untitled", "new row"]} []]

          stms
            `shouldBe` ( IntermediateInfo
                         { entity_info = M.fromList
                           [ ("A", EntityInfo {width = (5, 26.666666)})
                           , ("B", EntityInfo {width = (26.666666, 26.666666)})
                           , ("C", EntityInfo {width = (26.666666, 26.666666)})
                           , ("D", EntityInfo {width = (26.666666, 5)})
                           ]
                         , entity_txt  = M.fromList [("A", mkTxt "A"), ("B", mkTxt "B"), ("C", mkTxt "C"), ("D", mkTxt "D")]
                         }
                       , [GroupStmt ("A", "D") twoRowsTxt []]
                       )

      context "when parsing a group with a note above" $ do

        it "should range over all entities in the note and return note stmt" $ do
          let
            info       = buildInfo [("A", 5), ("B", 31.666666), ("C", 58.333332), ("D", 85)]
            Right stms = run
              fontTable
              info
              [ Group
                  ParserInfo {row = 1, column = 1, src = ""}
                  Text {t = ["untitled", "new row"]}
                  [ Note ParserInfo {row = 1, column = 1, src = ""}
                         NoteAbove
                         ["A", "B", "C", "D"]
                         Text {t = ["untitled", "new row"]}
                  ]
              ]

          stms
            `shouldBe` ( IntermediateInfo
                         { entity_info = M.fromList
                           [ ("A", EntityInfo {width = (5, 26.666666)})
                           , ("B", EntityInfo {width = (26.666666, 26.666666)})
                           , ("C", EntityInfo {width = (26.666666, 26.666666)})
                           , ("D", EntityInfo {width = (26.666666, 5)})
                           ]
                         , entity_txt  = M.fromList [("A", mkTxt "A"), ("B", mkTxt "B"), ("C", mkTxt "C"), ("D", mkTxt "D")]
                         }
                       , [ GroupStmt ("A", "D")
                                     twoRowsTxt
                                     [NoteStmt (AboveNote "A" "D") twoRowsTxt (V.fromList ["A", "B", "C", "D"])]
                         ]
                       )

      context "when group title width is larger than the one given by the stmts" $ do

        it "should range over all entities involved in the stmt and it should affect entities width" $ do
          let
            info       = buildInfo [("A", 5), ("B", 61.6666666667), ("C", 118.3333333334), ("D", 175)]
            Right stms = run
              fontTable
              info
              [ Group
                  ParserInfo {row = 1, column = 1, src = ""}
                  Text {t = ["untitled untitled", "new row"]}
                  [ Note ParserInfo {row = 1, column = 1, src = ""}
                         NoteAbove
                         ["A", "B", "C", "D"]
                         Text {t = ["untitled", "new row"]}
                  ]
              ]

          stms
            `shouldBe` ( IntermediateInfo
                         { entity_info = M.fromList
                           [ ("A", EntityInfo {width = (5, 56.6666666667)})
                           , ("B", EntityInfo {width = (56.6666666667, 56.6666666667)})
                           , ("C", EntityInfo {width = (56.6666666667, 56.6666666667)})
                           , ("D", EntityInfo {width = (56.6666666667, 5)})
                           ]
                         , entity_txt  = M.fromList [("A", mkTxt "A"), ("B", mkTxt "B"), ("C", mkTxt "C"), ("D", mkTxt "D")]
                         }
                       , [ GroupStmt
                             ("A", "D")
                             (Txt
                               { max_width = 170
                               , txt_info  = [ TxtInfo {txt_width = 170, txt_height = 52, txt = "untitled untitled"}
                                             , TxtInfo {txt_width = 70, txt_height = 52, txt = "new row"}
                                             ]
                               }
                             )
                             [NoteStmt (AboveNote "A" "D") twoRowsTxt (V.fromList ["A", "B", "C", "D"])]
                         ]
                       )
