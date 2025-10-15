{-# LANGUAGE RecursiveDo, ViewPatterns #-}
module AbsToIntermediate
  ( intermediate
  , Stmt(..)
  , NotePosition(..)
  , ReqStmt(..)
  , Txt(..)
  , TxtInfo(..)
  , EntityResume(..)
  , flowsToIntermediate
  )
where

import           AbsFlow                       as Abs
import           AbsData
import           Font
import           Env
import           Data.Bifunctor
import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           Control.Monad                  ( when )
import qualified Data.Map                      as M
import qualified Data.List                     as L
import qualified Data.Vector                   as V
import qualified Data.Set                      as S
import           Data.Foldable
import           Flow
import           Txt
import           Debug.Trace                    ( traceShowM
                                                , traceShow
                                                )
import           IntermediateEnv                ( intToFloat )
import           IntermediateInfo

type X = Float


data ArrowDirection
  = LeftArrow
  | RightArrow
  | SelfArrow
  deriving (Eq, Show)

data ArrowType
  = Thick
  | Dash
  deriving (Eq, Ord, Show)

type EntityPositions = M.Map String X

newtype EntityResume = EntityResume
  { position :: EntityPositions
  }
  deriving (Eq, Show)

data ArrowStmt = ArrowStmt
  { direction  :: ArrowDirection
  , arrow_type :: ArrowType
  } deriving (Eq, Show)

data Data = Data { d_width :: Float, d_height :: Float, d_data :: RequestData }
  deriving (Eq, Show)

type To = Txt
type From = Txt

data ReqStmt = ReqStmt To From Txt ArrowStmt (Maybe Data)
  deriving (Eq, Show)


data NotePosition = LeftNote EntityName | RightNote EntityName | AboveNote EntityName EntityName
 deriving (Eq, Show)

data Stmt
  = EmptyStmt
  -- | GenericStmt StmtInfo a
  | DestroyStmt EntityName
  | RequestStmt ReqStmt [Stmt] (Maybe ReqStmt)
  -- | OptRequest StmtInfo Float Float a [((Float,a),StmtBlock a)]
  | GroupStmt (EntityName,EntityName) Txt [Stmt]
  | NoteStmt NotePosition Txt Entities
  | EntityStmt EntityName Txt
  | IncludeStmt [Stmt]
  | DelayStmt Txt
  | TitleStmt Txt
 deriving (Eq, Show)


data WidthEncoding = WidthLeft Width | WidthCenter Width | WidthRight Width deriving Show

intermediate :: Font a => FontTable a -> Diagram b -> Either String [Stmt]
intermediate font diagram = evalState (runExceptT (diagramToIntermediate diagram)) emptyEnv where emptyEnv = Env font empty

diagramToIntermediate :: Font a => Diagram b -> IntermediateT a [Stmt]
diagramToIntermediate (DiagramRule _ flow) = do
  rec (info, elements) <- flowsToIntermediate (EntityResume newInfo) flow
      allEntities      <- all_entities <$> gets entities
      newInfo          <- computeXPos info allEntities
  return elements
 where
  flowAccumulator posInfo (info, stmts) item = bimap (info <>) (: stmts) <$> flowToIntermediate posInfo item

  computeXPos :: Font a => IntermediateInfo -> V.Vector EntityName -> IntermediateT a EntityPositions
  computeXPos table = foldlM (f <| entity_info table) mempty
   where
    f eMap info key = do
      prevEntity <- prevEntity key
      let prev                     = maybe mempty (eMap M.!) prevEntity
          entity                   = eMap M.! key
          (leftWidth, _          ) = width entity
          (_        , pRightWidth) = width prev
          xPos                     = maybe 0 (info M.!) prevEntity + max pRightWidth leftWidth
      return <| M.insert key xPos info

flowsToIntermediate :: Font a => EntityResume -> [Flow b] -> IntermediateT a (IntermediateInfo, [Stmt])
flowsToIntermediate info = foldM (flowAccumulator info) (mempty, [])
  where flowAccumulator posInfo (info, stmts) item = bimap (info <>) (: stmts) <$> flowToIntermediate posInfo item

flowToIntermediate :: Font a => EntityResume -> Flow b -> IntermediateT a (IntermediateInfo, Stmt)
flowToIntermediate info (Group _ title flow) = do
  txt                   <- computeTextM title
  (groupInfo, stmts)    <- flowsToIntermediate info flow
  groupInvolvedEntities <-
    (case flow of
        [] -> getActiveEntities
        _  -> return $ entitiesFromInfo groupInfo
      )
      >>= involvedEntities
  let len             = length groupInvolvedEntities
      neededMinWidth  = max_width txt / (if len == 1 then 2 else intToFloat (len - 1))
      leftmostEntity  = groupInvolvedEntities V.! 0
      rightmostEntity = groupInvolvedEntities V.! (len - 1)
  titleGroupEntities <- computeParticipantWidths groupInvolvedEntities [] (neededMinWidth, neededMinWidth)
  return (titleGroupEntities, GroupStmt (leftmostEntity, rightmostEntity) txt stmts)
flowToIntermediate info (Requests _ req) = intermediateReq info req
 where
  intermediateReq info (Request _ start stmts end) = do
    (openReqInfo, openReq          ) <- basicReqToIntermediate info start
    (reqInfo    , intermediateStmts) <- flowsToIntermediate info stmts
    closeReq                         <- traverse (basicReqToIntermediate info) end
    let resultInfo = maybe reqInfo fst closeReq
        endReq     = fmap snd closeReq
    return (reqInfo, RequestStmt openReq intermediateStmts endReq)
flowToIntermediate _ (Title _ txt) = do
  txt <- computeTextM txt
  return (mempty, TitleStmt txt)
flowToIntermediate _ (Participant _ entity _) = do
  txt <- computeTextM (linedText entity)
  addEntity entity
  return (mempty, EntityStmt entity txt)
flowToIntermediate _ (Delay _ txt) = do
  delayTxt <- computeTextM txt
  return (mempty, DelayStmt delayTxt)
flowToIntermediate info (Include _ _            _        flow   ) = second IncludeStmt <$> flowsToIntermediate info flow
flowToIntermediate _    (Note    _ notePosition entities noteTxt) = do
  mapM_ addEntity entities
  txt                 <- computeTextM noteTxt
  allEntitiesInvolved <- involvedEntities entities
  entitiesNoteInfo    <- joinEntityInfos notePosition allEntitiesInvolved (max_width txt)
  return (entitiesNoteInfo, NoteStmt (computeNotePosition notePosition allEntitiesInvolved) txt allEntitiesInvolved)
 where

  joinEntityInfos :: (Font a) => StartNote -> V.Vector EntityName -> Width -> IntermediateT a IntermediateInfo
  joinEntityInfos NoteAbove entities max_width = do
    let len            = length entities
        neededMinWidth = max_width / (if len == 1 then 2 else intToFloat (len - 1))
    computeParticipantWidths entities [] (neededMinWidth, neededMinWidth)
  joinEntityInfos pos entities max_width =
    let widthInfo = case pos of
          NoteLeft  -> (max_width, 0)
          NoteRight -> (0, max_width)
    in  computeParticipantWidths entities [] widthInfo

  computeNotePosition :: StartNote -> Entities -> NotePosition
  computeNotePosition pos entity =
    let fistEntity = (entity V.! 0)
    in  case pos of
          NoteLeft  -> LeftNote fistEntity
          NoteRight -> RightNote fistEntity
          NoteAbove -> AboveNote fistEntity (entity V.! (V.length entity - 1))
flowToIntermediate info (Destroy _ entity) = let x = position info M.! entity in return (mempty, DestroyStmt entity)
flowToIntermediate _    _                  = return (mempty, EmptyStmt)

basicReqToIntermediate :: Font a => EntityResume -> BasicRequest b -> IntermediateT a (IntermediateInfo, ReqStmt)
basicReqToIntermediate info (BasicRequest _ (RequestEntity req _) arrow (RequestEntity res _) label reqData) = do
  mapM_ addEntity [req, res]
  allEntitiesInvolved <- involvedEntities [req, res]
  reqEntity           <- computeStringM req
  resEntity           <- computeStringM res
  labelTxt            <- computeTextM label
  return (mempty, ReqStmt reqEntity resEntity labelTxt (arrowInfo info arrow req res) Nothing)
 where

  arrowInfo info arrow req res =
    let reqPos = position info M.! req
        resPos = position info M.! res
    in  ArrowStmt {direction = computeArrowDirection reqPos resPos, arrow_type = extractArrowType arrow}
   where
    computeArrowDirection req res | req == res = SelfArrow
                                  | req > res  = LeftArrow
                                  | otherwise  = RightArrow

    extractArrowType arrow =
      let ty = case arrow of
            Abs.Arrow                ty -> ty
            NewParticipantArrow      ty -> ty
            NewParticipantBlockArrow ty -> ty
            NewBlockArrow            ty -> ty
            ClosingBlockArrow        ty -> ty
            FailedArrow              ty -> ty
      in  mapArrowType ty
     where
      mapArrowType Abs.Thick = AbsToIntermediate.Thick
      mapArrowType Abs.Dash  = AbsToIntermediate.Dash

computeParticipantWidths :: Font a => V.Vector EntityName -> [String] -> (Float, Float) -> IntermediateT a IntermediateInfo
computeParticipantWidths (V.uncons -> Nothing               ) _      _        = return mempty
computeParticipantWidths (V.uncons -> Just (entity,entities)) window (lw, rw) = do
  txt <- computeTextM (linedText entity)
  let entityHalfWidth = max_width txt / 2
      leftWidth       = case (entities, window) of
        (V.uncons -> Just _, []) -> entityHalfWidth -- first of a list of elements 
        _                        -> max entityHalfWidth lw
      rightWidth = case (entities, window) of
        (V.uncons -> Nothing, _ : _) -> entityHalfWidth -- last element of the list
        _                            -> max entityHalfWidth rw
  (buildEntityInfo entity txt (leftWidth, rightWidth) <>) <$> computeParticipantWidths entities (entity : window) (lw, rw)

buildEntityInfo :: String -> Txt -> (Width, Width) -> IntermediateInfo
buildEntityInfo entityName txt entityWidth =
  IntermediateInfo {entity_txt = M.singleton entityName txt, entity_info = M.singleton entityName (EntityInfo entityWidth)}

computeTextM :: Font a => Text -> IntermediateT a Txt
computeTextM txt = do
  font <- gets font
  return <| computeText font txt

computeStringM :: Font a => String -> IntermediateT a Txt
computeStringM txt = do
  font <- gets font
  return <| computeString font txt




-- data RequestData
--   = Json JsonValue
--   | Form [Assignment]
--   deriving (Eq, Ord, Show)

-- data Value
--   = BoolValue Bool
--   | IntegerValue Integer
--   | StringValue String
--   deriving (Eq, Ord, Show)

-- newtype Identifier =
--   Identifier String
--   deriving (Eq, Ord, Show)

-- data Assignment
--   = AssignmentByValue Identifier
--                       Value
--   | AssignmentById Identifier
--                    Identifier
--   deriving (Eq, Ord, Show)

-- data JsonValue
--   = JsonString String
--   | JsonObject [(String,JsonValue)]
--   | JsonArray [JsonValue]
--   | JsonInteger Integer
--   | JsonBoolean Bool
--   | JsonNullTk
--   | JsonDouble Float
--   | JsonIdentifier Identifier
--   deriving (Eq, Ord, Show)
