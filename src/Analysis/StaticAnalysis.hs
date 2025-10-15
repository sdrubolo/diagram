module StaticAnalysis
  ( check
  )
where
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Foldable
import qualified Data.Set                      as Set
import qualified Data.List                     as List
import           AbsFlow                       as Abs
import           Flow
import           Debug.Trace
import           AbsError                      as Err

newtype Env =
   Env
     { activeEntities :: Set.Set String }


type Checker = ExceptT Error (State Env)

emptyEnv = Env {activeEntities = Set.empty}

infoToError :: ParserInfo -> String -> Error
infoToError ParserInfo {..} message = Error {row = row, column = column, message = message, src = src}

getActiveParticipants :: Checker (Set.Set String)
getActiveParticipants = gets activeEntities

setActiveParticipant :: String -> Checker ()
setActiveParticipant entity = modify (addEntity entity)
  where addEntity entity env@Env {..} = env { activeEntities = Set.insert entity activeEntities }

unsetActiveParticipant :: String -> Checker ()
unsetActiveParticipant entity = modify (removeEntity entity)
  where removeEntity entity env@Env {..} = env { activeEntities = Set.delete entity activeEntities }

isActiveParticipant :: String -> Checker Bool
isActiveParticipant entity = do
  active <- gets activeEntities
  return <| Set.member entity active

check :: Diagram ParserInfo -> Either Err.Error (Diagram ParserInfo)
check diagram = evalState (runExceptT (checkDiagram diagram)) emptyEnv

checkDiagram :: Diagram ParserInfo -> Checker (Diagram ParserInfo)
checkDiagram diagram@(DiagramRule _ flows) = do
  mapM_ checkStmt flows
  return diagram

checkStmt :: Flow ParserInfo -> Checker (Flow ParserInfo)
checkStmt group@(Group info txt stmts) = do
  activeEntities <- getActiveParticipants
  when (null stmts || Set.null activeEntities) <| throwError <| infoToError info
                                                                            "Group is empty and not participant is defined"
  mapM_ checkStmt stmts
  return group
checkStmt request@(Requests _ req) = do
  checkRequest req
  return request
checkStmt set@(SetTo info identifier value) = do
  isResponseActive <- isActiveParticipant identifier
  unless isResponseActive <| throwError <| infoToError info ("Participant " <> identifier <> " is not active")
  return set
checkStmt header@(Header info (Head head _)) = do
  isResponseActive <- isActiveParticipant head
  unless isResponseActive <| throwError <| infoToError info ("Participant " <> head <> " is not active")
  return header
checkStmt participant@(Participant info name path) = do
  when (null name) <| throwError <| infoToError info "Participant cannot be empty"
  setActiveParticipant name
  return participant
checkStmt include@(Include _ path subts stmts) = do
  mapM_ checkStmt stmts
  return include
checkStmt note@(Note _ position entities txt) = do
  mapM_ setActiveParticipant entities
  return note
checkStmt destroy@(Destroy info entity) = do
  isResponseActive <- isActiveParticipant entity
  unless isResponseActive <| throwError <| infoToError info ("Participant " <> entity <> " is not active")
  unsetActiveParticipant entity
  return destroy
checkStmt delay@(Delay info _) = do
  activeEntities <- getActiveParticipants
  when (Set.null activeEntities) <| throwError <| infoToError info "Delay cannot be used when participants are not defined"
  return delay
checkStmt title@(       Title _ _                         ) = return title
checkStmt groupRequest@(LabelledGroup info groupType stmts) = do
  mapM_ checkLabelling stmts
  activeEntities <- getActiveParticipants
  when (null stmts || null activeEntities) <| throwError <| infoToError
    info
    "Optional group is empty and not participant is defined"
  return groupRequest

checkRequest :: Request ParserInfo -> Checker (Request ParserInfo)
checkRequest request@(Request _ req1 [] Nothing) = do
  checkBasicRequest req1
  return request
checkRequest (Request info _ _ Nothing) =
  throwError <| infoToError info "Impossible happened nesting applied to simple request"
checkRequest request@(Request info req1 nest (Just req2)) = do
  (requestEntities, _, _) <- checkBasicRequest req1
  mapM_ checkStmt nest
  (responseEntities, infoResponse, _) <- checkBasicRequest req2
  let [_                  , endRequestEntity] = requestEntities
      [startResponseEntity, _               ] = responseEntities
  when (endRequestEntity /= startResponseEntity) <| throwError <| infoToError
    infoResponse
    ("Participant " <> startResponseEntity <> " is not active")
  return request

checkBasicRequest :: BasicRequest ParserInfo -> Checker ([String], ParserInfo, BasicRequest ParserInfo)
checkBasicRequest basic@(BasicRequest info (RequestEntity req _) (NewParticipantArrow _) (RequestEntity res _) _ _) = do
  isResponseActive <- isActiveParticipant res
  when isResponseActive <| throwError <| infoToError info ("Participant " <> res <> " is already active")
  mapM_ setActiveParticipant [req, res]
  return ([req, res], info, basic)
checkBasicRequest basic@(BasicRequest info (RequestEntity req _) _ (RequestEntity res _) _ _) = do
  mapM_ setActiveParticipant [req, res]
  return ([req, res], info, basic)

checkLabelling :: Labelling ParserInfo -> Checker (Labelling ParserInfo)
checkLabelling l@(Labelling _ txt stmts) = do
  mapM_ checkStmt stmts
  return l



