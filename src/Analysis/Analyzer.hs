module Analyzer
  ( tag
  , tagStmt
  --, mkStmtExpr
  , TagInfo(..)
  , Info(..)
  , EntityState(..)
  , EntitiesExpr(..)
  , EntitiesSet(..)
  )
where
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Foldable
import qualified Data.List                     as L
import qualified Data.Sequence                 as S
import qualified Data.Map                      as M
import           AbsFlow                       as Abs
import           AbsError                      as Err
import           Flow
import           Debug.Trace
import           Expr
import           IntermediateEnv                ( Box(b_height)
                                                , Block(propagation)
                                                )
import qualified Control.Arrow                 as A
import qualified Control.Category              as Cat


data EntityState = On | Off
  deriving (Show, Eq)

type EntitiesSet = [(EntityName,EntityState)]

type EntitiesExpr = Expr EntitiesSet

data Info a b = Info {
  _in :: a,
  _out :: b
}
  deriving (Show, Eq)

type TagInfo = Info EntitiesSet EntitiesSet

insert :: EntityName -> [(EntityName, EntityState)] -> [(EntityName, EntityState)]
insert a []              = [(a, On)]
insert a (e@(x, s) : xs) = if a == x then (x, On) : xs else e : insert a xs

remove :: EntityName -> [(EntityName, EntityState)] -> [(EntityName, EntityState)]
remove a []              = []
remove a (e@(x, s) : xs) = if a == x then (x, Off) : xs else e : remove a xs

buildTagInfo :: Propagation EntitiesSet -> EntitiesSet -> EntitiesSet -> TagInfo
buildTagInfo propagation t b = Info (top propagation t) (bottom propagation b)

tag :: Diagram a -> Diagram TagInfo
tag (DiagramRule _ flows) =
  let Expr { propagation = reqProgation, expr = reqExpr } = foldMap tagStmt flows
      diagram_info    = Info top_entities bottom_entities
      top_entities    = top reqProgation []
      bottom_entities = bottom reqProgation []
  in  DiagramRule diagram_info (reqExpr [] [] [])

tagStmt :: Flow a -> EntitiesExpr [Flow TagInfo]
tagStmt (Group _ txt stmts) =
  let Expr { propagation = groupProgation, expr = groupExpr } = foldMap tagStmt stmts
  in  Expr groupProgation (\t b -> (Group (buildTagInfo groupProgation t b) txt (groupExpr t b []) :))
tagStmt (Requests _ req) =
  let (reqProgation, reqExpr) = tagRequest req
  in  Expr reqProgation (\t b -> (Requests (buildTagInfo reqProgation t b) (reqExpr t b) :))
tagStmt (SetTo _ identifier value) = mempty { expr = \t b -> (SetTo (Info t b) identifier value :) }
tagStmt (Header _ head           ) = mempty { expr = \t b -> (Header (Info t b) head :) }
tagStmt (Participant _ name path) =
  let propagation = Propagation addParticipant addParticipant
  in  Expr propagation (\t b -> (Participant (buildTagInfo propagation t b) name path :))
  where addParticipant = insert name
tagStmt (Include _ path subts stmts) = foldMap tagStmt stmts
tagStmt (Note _ position entities txt) =
  let propagation = Propagation addParticipants addParticipants
  in  Expr propagation (\t b -> (Note (buildTagInfo propagation t b) position entities txt :))
  where addParticipants = foldr (\entity acc -> acc . insert entity) id entities
tagStmt (Destroy _ entity) = Expr destroy_propagation (\t b -> (Destroy (buildTagInfo destroy_propagation t b) entity :))
  where destroy_propagation = mempty { bottom = remove entity }
tagStmt (Delay _ txt) = mempty { expr = \t b -> (Delay (Info t b) txt :) }
tagStmt (Title _ txt) = mempty { expr = \t b -> (Title (Info t b) txt :) }
tagStmt (LabelledGroup _ groupType stmts) =
  let Expr { propagation = labellingProgation, expr = labellingExpr } = foldMap tagLabelling stmts
  in  Expr labellingProgation
           (\t b -> (LabelledGroup (buildTagInfo labellingProgation t b) groupType (labellingExpr t b []) :))

tagLabelling :: Labelling a -> EntitiesExpr [Labelling TagInfo]
tagLabelling (Labelling _ txt stmts) =
  let Expr { propagation = labellingProgation, expr = labellingExpr } = foldMap tagStmt stmts
  in  Expr labellingProgation (\t b -> (Labelling (buildTagInfo labellingProgation t b) txt (labellingExpr t b []) :))

tagRequest :: Request a -> (Expr.Propagation EntitiesSet, EntitiesSet -> EntitiesSet -> Request TagInfo)
tagRequest request@(Request _ req nest res) =
  let (reqProgation, reqExpr) = tagBasicRequest req
      Expr { propagation = nestProgation, expr = nestExpr } = foldMap tagStmt nest
      resExpr                 = tagBasicRequest <$> res
      propagation@Propagation { top = t_p, bottom = b_p } = maybe mempty fst resExpr <> nestProgation <> reqProgation
  in  ( propagation
      , \t b ->
        let req_top    = t_p t
            req_bottom = b_p b
        in  Request (buildTagInfo propagation t b)
                    (reqExpr req_top req_bottom)
                    (nestExpr req_top req_bottom [])
                    (snd <$> resExpr <*> pure req_top <*> pure req_bottom)
      )

tagBasicRequest :: BasicRequest a -> (Expr.Propagation EntitiesSet, EntitiesSet -> EntitiesSet -> BasicRequest TagInfo)
tagBasicRequest basic@(BasicRequest _ rq@(RequestEntity req _) arrow@(NewParticipantArrow _) rs@(RequestEntity res _) txt reqData)
  = (reqPropagation, \t b -> BasicRequest (buildTagInfo reqPropagation t b) rq arrow rs txt reqData)
 where
  topPropagation    = insert res
  bottomPropagation = topPropagation . insert req
  reqPropagation    = Propagation topPropagation bottomPropagation
tagBasicRequest basic@(BasicRequest _ rq@(RequestEntity req _) arrow@(ClosingBlockArrow _) rs@(RequestEntity res _) txt reqData)
  = (reqPropagation, \t b -> BasicRequest (buildTagInfo reqPropagation t b) rq arrow rs txt reqData)
 where
  propagation    = insert req . insert res
  reqPropagation = Propagation propagation propagation
tagBasicRequest basic@(BasicRequest _ rq@(RequestEntity req _) arrow rs@(RequestEntity res _) txt reqData) =
  (reqPropagation, \t b -> BasicRequest (buildTagInfo reqPropagation t b) rq arrow rs txt reqData)
 where
  propagation    = insert res . insert req
  reqPropagation = Propagation propagation propagation
