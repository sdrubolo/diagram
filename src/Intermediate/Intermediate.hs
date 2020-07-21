
{-# LANGUAGE RecursiveDo           #-}

module Intermediate
  ( draw
  )
where

import           AbsFlow                       as Abs
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.IfElse
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Foldable
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import qualified Data.Set                      as Set
import           Debug.Trace
import           Flow
import           Font
import           IntermediateAbs
import           IntermediateData
import           IntermediateEnv
import           Stmt



type DrawInfo = IntermediateInfo EntityStore

instance Semigroup DrawInfo where
  (<>) a b = DrawInfo { entityInfo = Map.unionWith (<>) (entityInfo a) (entityInfo b) }

instance Monoid DrawInfo where
  mempty = DrawInfo { entityInfo = Map.empty }

data RequestPosition =
  Position String
           Float
           [Maybe ArrowDirection]
  deriving (Show)

blockWidth :: Float -> String -> ArrowDirection -> ReqBlock -> Float
blockWidth defaultBlock entity direction block = foldr (\d a -> a + maybe defaultBlock (sameDirection direction) d)
                                                       0
                                                       (Map.findWithDefault [] entity block)
 where
  sameDirection L R = 10
  sameDirection R blockDirection | blockDirection /= R = 10
  sameDirection _ _ = 0

getX :: RequestPosition -> Float
getX (Position _ x block) = x + foldr (\d a -> a + maybe 0 toInt d) 0 block
 where
  toInt R = -10
  toInt _ = 10

blockOrientedX :: RequestPosition -> ArrowDirection -> Float
blockOrientedX p@(Position _ _ []) _         = getX p
blockOrientedX p                   direction = getX p + case direction of
  L -> -10
  _ -> 10

getLabelXPos :: RequestPosition -> ArrowDirection -> Float
getLabelXPos r@(Position _ x []) d = blockOrientedX r d
getLabelXPos r@(Position l x b ) d = f (blockOrientedX (Position l x [Nothing]) d) (blockOrientedX r d)
 where
  f = case d of
    L -> min
    _ -> max

makeRequestPosition info l v = return <| Position l (getEntityXPos info l) (Map.findWithDefault [] l v)

data RequestBlock = RequestBlock
  { from      :: RequestPosition
  , to        :: RequestPosition
  , label     :: (Float, Expr)
  , payload   :: (Float, Expr)
  , direction :: ArrowDirection
  , arrowType :: Arrows
  , participant :: (Maybe (EntityName,EntityInfo),BlockPropagation)
  }

toString :: GroupType -> String
toString Loop        = "loop"
toString Optional    = "opt"
toString Alternative = "alt"

addEntityTo :: EntityName -> DrawInfo -> EntityInfo -> IntermediateT a DrawInfo
addEntityTo entity info eData = return <| addEntityWith (<>) entity info eData
 where
  addEntityWith :: (EntityInfo -> EntityInfo -> EntityInfo) -> EntityName -> DrawInfo -> EntityInfo -> DrawInfo
  addEntityWith f entity info eData = info { entityInfo = Map.insertWith f entity eData <| entityInfo info }

addToActiveEntities :: EntityName -> IntermediateT a ()
addToActiveEntities entity = modify (\env -> env { scope = Set.insert entity (scope env) })

deleteFromActiveEntities :: EntityName -> IntermediateT a ()
deleteFromActiveEntities entity = modify (\env -> env { scope = Set.delete entity (scope env) })

getEntity :: DrawInfo -> EntityName -> EntityInfo
getEntity info entity = entityInfo info Map.! entity

getEntityXPos :: DrawInfo -> EntityName -> Float
getEntityXPos info entity = x (getEntity info entity)

getBoundaryEntities :: Font a => [String] -> IntermediateT a (String, String)
getBoundaryEntities allEntities = do
  sortedEntities <- flip intersect allEntities <$> getSortedEntities
  return (head sortedEntities, last sortedEntities)

getEntityNames :: DrawInfo -> IntermediateT a [String]
getEntityNames info = flip intersect (Map.foldrWithKey (\e _ acc -> e : acc) [] <| entityInfo info) <$> getSortedEntities

mkStartDrawInfo :: Font a => Map.Map String EntityDef -> IntermediateT a DrawInfo
mkStartDrawInfo eInfo = do
  rec (maxHeight, info) <- foldlM (mkEntityInfos info maxHeight) (0, mempty) eInfo
  return info
 where
  mkEntityInfos computedInfo maxEntityHeight (max_height, drawInfo) entity@EntityDef { name = entityName, text = entityText }
    = do
      rec (entityWidth, txt) <- linedText entityText (-entityWidth / 2)
          leftEntity         <- getLeftSideEntity entityName
      boxWidth  <- gets boxWidth
      boxHeight <- gets boxHeight
      let w                   = max boxWidth <| entityWidth + 30
          computedEntityWidth = w / 2
          box_height          = max (evalHeight txt + 10) boxHeight
          def                 = mempty { width = (computedEntityWidth, computedEntityWidth)
                                       , x     = 0
                                       , box   = Just <| Box {b_height = maxEntityHeight, b_width = w}
                                       , txt   = Just txt
                                       }
      info <- addEntityTo entityName mempty def
      return (max max_height box_height, drawInfo <> info)

draw :: Font a => FontTable a -> EntityTable -> Diagram b -> Either String Draw
draw font table diagram = evalState (runExceptT (drawDiagram table diagram)) (initEnv table font)

drawDiagram :: Font a => EntityTable -> Diagram b -> IntermediateT a Draw
drawDiagram EntityTable {..} (DiagramRule _ flows) = do
  font     <- gets font
  drawInfo <- mkStartDrawInfo entitiesMap
  rec (info, elements) <- drawStmts (DrawInfo {entityInfo = newInfo}) Map.empty flows
      newInfo          <- computeXPos (entityInfo (drawInfo <> info)) allEntities
  return <| diagram font newInfo elements
 where

  computeXPos :: Font a => Map.Map EntityName EntityInfo -> [EntityName] -> IntermediateT a (Map.Map EntityName EntityInfo)
  computeXPos table entities = snd <$> f 0 table entities
   where
    f x _    []           = return (x, mempty)
    f x eMap (key : keys) = do
      nextEntity <- maybe mempty (eMap Map.!) <$> nextEntity key
      let entity                   = eMap Map.! key
          (leftWidth , rightWidth) = width entity
          (nLeftWidth, _         ) = width nextEntity
          xPos                     = x + leftWidth
          minWidth                 = 30 + boxWidth entity / 2 + boxWidth nextEntity / 2
          rWidth                   = (max minWidth <| 10 + max rightWidth nLeftWidth) - nLeftWidth
      (nextX, nextInfo) <- f (xPos + rWidth) eMap keys
      return (nextX, Map.insert key (entity { x = xPos }) nextInfo)
      where boxWidth entity = maybe 0 (\b -> b_width b) (box entity)


diagram :: FontTable a -> Map.Map EntityName EntityInfo -> Stmt -> Draw
diagram font drawInfo diagramStmt =
  let
    totalWidth              = computeWidth drawInfo
    documentWidth           = totalWidth + 150
    (titleWidth, titleExpr) = second (translate titleCentering 0) <| title <| stmtInfo diagramStmt
    titleCentering          = (documentWidth - titleWidth) / 2 - documentCentering
    documentCentering       = (documentWidth - totalWidth) / 2
    (diagramHeight, diagram) =
      blockProgress ((drawSpace 40 <~> titleExpr) <> expr (EntityStmt mempty <> diagramStmt <> EntityStmt mempty)) drawInfo 0
    documentHeight = 20 + diagramHeight
  in
    Draw [Attr "width" (AttrFloat documentWidth), Attr "height" (AttrFloat documentHeight)]
         [defs font <> IGroup [Transform <| Translate documentCentering 0] [diagram]]
 where
  computeWidth = Map.foldl (\w el -> max w <| x el + snd (width el)) 0
  defs font = Defs
    [ IGroup
      [Attr "id" (AttrStr "normalArrowRight"), Transform <| Translate (-10) (-10)]
      [ IPath
          [ Attr "d"            (AttrStr "M 0 4 L 10 10 L 0 16 L 0 4")
          , Attr "stroke"       (AttrStr "black")
          , Attr "stroke-width" (AttrStr "1")
          , Attr "fill"         (AttrStr "lightgray")
          ]
      ]
    , IGroup
      [Attr "id" <| AttrStr "x"]
      [ ILine
        [ Attr "x1" <| AttrFloat (-5)
        , Attr "y1" <| AttrFloat 5
        , Attr "x2" <| AttrFloat 5
        , Attr "y2" <| AttrFloat (-5)
        , Attr "stroke" <| AttrColor Black
        , Attr "stroke-width" <| AttrFloat 1.5
        ]
      , ILine
        [ Attr "x1" <| AttrFloat (-5)
        , Attr "y1" <| AttrFloat (-5)
        , Attr "x2" <| AttrFloat 5
        , Attr "y2" <| AttrFloat 5
        , Attr "stroke" <| AttrColor Black
        , Attr "stroke-width" <| AttrFloat 1.5
        ]
      ]
    , IStyle [Type "text/css"] [FontFace (show font) (Font.src font) (fontType font)]
    , IGroup [Attr "id" (AttrStr "normalArrowLeft")] [IUse [Transform <| Rotate 180] "normalArrowRight"]
    , IGroup [Attr "id" (AttrStr "normalArrowSelf")]   [IUse [] "normalArrowLeft"]
    , IGroup [Attr "id" (AttrStr "missingArrowSelf")]  [IUse [] "x"]
    , IGroup [Attr "id" (AttrStr "missingArrowLeft")]  [IUse [] "x"]
    , IGroup [Attr "id" (AttrStr "missingArrowRight")] [IUse [] "x"]
    , IPattern
      [ Attr "id"               (AttrStr "pinstripe")
      , Attr "patternUnits"     (AttrStr "userSpaceOnUse")
      , Attr "width"            (AttrStr "3.5")
      , Attr "height"           (AttrStr "50")
      , Attr "patternTransform" (AttrStr "rotate(45)")
      ]
      [ ILine
          [ Attr "x1"           (AttrInt 0)
          , Attr "y1"           (AttrInt 0)
          , Attr "x2"           (AttrInt 0)
          , Attr "y2"           (AttrInt 50)
          , Attr "stroke"       (AttrStr "black")
          , Attr "stroke-width" (AttrStr "0.5")
          ]
      ]
    ]

drawStmts :: Font a => DrawInfo -> ReqBlock -> [Flow b] -> IntermediateT a (DrawInfo, Stmt)
drawStmts info blocks = foldlM (\(i, els) flow -> bimap (i <>) (els <>) <$> drawElement info blocks flow) (mempty, mempty)

drawParticipant :: EntityInfo -> Expr
drawParticipant EntityInfo { box = box, x = x_pos, txt = Just blockText } =
  let width   = maybe 0 b_width box
      box_pos = x_pos - width / 2
      h       = maybe 0 b_height box
  in  attachTextToBox (rectangle width h box_pos 1.0 1.0 Black) (translate x_pos 0 blockText)

drawElement :: Font a => DrawInfo -> ReqBlock -> Flow b -> IntermediateT a (DrawInfo, Stmt)
drawElement info blocks (Title _ title) = do
  (titleWidth, titleExpr) <- linedText title 0
  return <| (mempty, TitleStmt (mempty { title = (titleWidth, drawSpace_20 <> titleExpr <> drawSpace_20) }))
drawElement info blocks (     Requests _ request    ) = drawRequest blocks info request
drawElement info blocks step@(Abs.Group _ text stmts) = do
  (groupInfo, groupRequests) <- drawStmts info blocks stmts
  rec (eInfo, (leftmost, rightmost)) <- getGroupCoordinates groupInfo blocks (width + 30)
      (width, txt                  ) <- linedText text xText
      let xText  = x1 + (x2 - x1 - width) / 2
          x1     = getEntityXPos info leftmost
          x2     = getEntityXPos info rightmost
          lineX1 = x1 - 20
          lineX2 = x2 + 20
  return
    ( eInfo
    , GroupStmt (mempty { block = blocks })
                lineX1
                lineX2
                (attachTextToBox (rectangle width (evalHeight txt + 10) xText 1.0 1.0 White) txt)
                groupRequests
    )

drawElement info blocks (Note _ position entitiesNames text) = do
  rec (txt_width, content) <- linedText text (x_pos_fn info + (width_fn info - txt_width) / 2)
      esMap                <- getPreviousEntitiesAndItself entitiesNames
      (width_fn, x_pos_fn, involved_entities) <- postionInfo position esMap (txt_width + 15)
  let entitySet = foldl (\acc el -> acc . Map.insert el (getEntity info el)) id entitiesNames
      box       = rectangle (width_fn info) (evalHeight content + 20) (x_pos_fn info) 1.0 1.0 Black
  eInfo <- foldrM (\(e, w) acc -> addEntityTo e acc (mempty { width = w })) mempty involved_entities
  mapM_ (addToActiveEntities . fst) involved_entities
  return
    ( eInfo
    , NoteStmt (mempty { block = blocks })
    <| mempty { propagation = Propagation {top = entitySet, bottom = entitySet} }
    <> attachTextToBox box content
    )
 where
  postionInfo
    :: Font a
    => StartNote
    -> [(EntityName, [EntityName])]
    -> Float
    -> IntermediateT a (DrawInfo -> Float, DrawInfo -> Float, [(EntityName, (Float, Float))])
  postionInfo p eMap width | NoteAbove <- p, [(s_name, s), (e_name, e)] <- eMap = do
    let involvedEntities = e \\ s
        entityWidth      = width / intToFloat (length involvedEntities)
    return
      ( \info -> getEntityXPos info e_name + 10 - getEntityXPos info s_name + 10
      , \info -> getEntityXPos info s_name - 10
      , map (\e -> (e, (0, entityWidth))) involvedEntities
      )
  postionInfo p eMap width | [(name, _)] <- eMap = do
    let (xAdjustment, widthAdd) = case p of
          NoteLeft  -> (-width - 10, (width + 30, 0))
          NoteRigth -> (10, (0, width + 20))
          NoteAbove -> (-width / 2, (width / 2, width / 2))
    return (const width, \info -> getEntityXPos info name + xAdjustment, [(name, widthAdd)])

drawElement info blocks (Include _ _ _ flows) = drawStmts info blocks flows
drawElement info blocks (Destroy _ entity   ) = do
  deleteFromActiveEntities entity
  return (mempty, DestroyStmt (mempty { block = blocks }) (Set.singleton entity) EmptyStmt)
drawElement _ _ (Participant _ participant _) = do
  addToActiveEntities participant
  (, mempty) <$> addEntityTo participant mempty mempty
drawElement info blocks (Delay _ txt) = do
  allInvolvedEntities   <- Set.toList <$> gets scope
  (leftmost, rightmost) <- getBoundaryEntities allInvolvedEntities
  rec (width, text) <- linedText txt xText
      let xText                          = start + (end - start - width) / 2
          start                          = getEntityXPos info leftmost
          end                            = getEntityXPos info rightmost
          EntityInfo { width = (x1, _) } = getEntity info leftmost
          EntityInfo { width = (_, x2) } = getEntity info rightmost
  let allInvolvedEntitiesButLast = allInvolvedEntities \\ [rightmost]
  rightInfo <- addEntityTo rightmost mempty mempty
  eInfo     <- addTextWidth rightInfo width allInvolvedEntitiesButLast
  return <| (eInfo, DelayStmt (mempty { block = blocks }) (start - x1) (end + x2) text)

drawElement _ _ _ = return (mempty, mempty)

addTextWidth :: DrawInfo -> Float -> [EntityName] -> IntermediateT a DrawInfo
addTextWidth info width entities =
  foldrM (\e acc -> addEntityTo e acc (mempty { width = (0, width / intToFloat (length entities)) })) info entities

drawRequest :: Font a => ReqBlock -> DrawInfo -> (Abs.Request b) -> IntermediateT a (DrawInfo, Stmt)
drawRequest blocks drawInfo (GroupRequest _ groupTy reqs) = do
  groupReqs <- mapM (drawLabeledGroupRequest drawInfo blocks) reqs
  drawGroupRequests blocks drawInfo (toString groupTy) groupReqs
drawRequest blocks drawInfo (Abs.Request _ request nesting response) = do
  newNestingBlock        <- updateNestingRequestBlock blocks request
  (reqDrawInfo, reqInfo) <- buildRequest drawInfo (blocks, newNestingBlock) <| Just request
  (nInfo      , nst    ) <- drawStmts drawInfo newNestingBlock nesting
  (resDrawInfo, resInfo) <- buildRequest drawInfo (newNestingBlock, newNestingBlock) response
  rec (_, req) <- makeReqExpr drawInfo reqInfo (blocks, newNestingBlock) 0
      (y, res) <- makeReqExpr drawInfo resInfo (newNestingBlock, blocks) (-y)
  return (reqDrawInfo <> resDrawInfo <> nInfo, RequestStmt (mempty { block = blocks }) mempty req nst res EmptyStmt)
 where
  makeReqExpr _    Nothing    _      _ = return (0, Empty)
  makeReqExpr info (Just req) blocks y = composeComponents info y req blocks


  updateNestingRequestBlock :: Font a => ReqBlock -> (BasicRequest b) -> IntermediateT a ReqBlock
  updateNestingRequestBlock blocks (BasicRequest _ (RequestEntity req _) (NewBlockArrow _) (RequestEntity res _) _ _) = do
    dir <- computeDirection req res
    return <| Map.insertWith (\_ v -> Just dir : v) res [Nothing] blocks
  updateNestingRequestBlock blocks _ = return blocks

drawGroupRequests
  :: Font a => ReqBlock -> DrawInfo -> String -> [(DrawInfo, (Float, Expr), Stmt)] -> IntermediateT a (DrawInfo, Stmt)
drawGroupRequests blocks drawInfo groupTitle groups = do
  rec (txtWidth, altInfo, reqs)           <- buildStackRequest (startX + boxWidth + 5) groups
      (coordsInfo, (leftmost, rightmost)) <- getGroupCoordinates altInfo blocks txtWidth
      let left@EntityInfo { width = (x1, _) }  = getEntity coordsInfo leftmost
          right@EntityInfo { width = (_, x2) } = getEntity coordsInfo rightmost
          leftMostX  = getEntityXPos drawInfo leftmost
          rightMostX = getEntityXPos drawInfo rightmost
          newX1      = x1 + 10 + if x1 + 10 < boxWidth then boxWidth else 0
          newX2      = x2 + 10
          startX     = leftMostX - newX1
          width      = max (txtWidth + boxWidth + 10) <| rightMostX + newX2 - startX
      (boxWidth, boxTitle) <- drawBoxTitle startX groupTitle
  leftInfo  <- addEntityTo leftmost mempty (left { width = (newX1, 0) })
  rightInfo <- addEntityTo rightmost mempty (right { width = (0, newX2) })
  return (coordsInfo <> leftInfo <> rightInfo, OptRequest (mempty { block = blocks }) startX width boxTitle reqs)
 where

  buildStackRequest
    :: Float -> [(DrawInfo, (Float, Expr), Stmt)] -> IntermediateT a (Float, DrawInfo, [((Float, Expr), Stmt)])
  buildStackRequest xPos = foldrM (buildGroupInfo xPos) (0, mempty, [])
   where
    buildGroupInfo xPos (info, txt@(w, _), req) (accWidth, accInfo, groups) = return
      (max accWidth w, accInfo <> info, ((w, translate xPos 0 label), req) : groups)
      where label = drawAltLabel txt

  drawAltLabel (width, txt) = attachTextToBox (rectangle width (evalHeight txt) 0 1.0 1.0 White) txt

getGroupCoordinates :: Font a => DrawInfo -> ReqBlock -> Float -> IntermediateT a (DrawInfo, (String, String))
getGroupCoordinates info blocks totalWidth = do
  (auxInfo, borders@(_, rightmost), allInvolvedEntities) <- aux info blocks
  let allInvolvedEntitiesButLast = case allInvolvedEntities of
        [_] -> allInvolvedEntities
        _   -> allInvolvedEntities \\ [rightmost]
  eInfo <- addTextWidth mempty totalWidth allInvolvedEntitiesButLast
  return (auxInfo <> eInfo, borders)
 where
  aux info blocks | Map.null (entityInfo info) = do
    scope <- gets scope
    let allInvolvedEntities = Set.toList scope
    (leftmost, rightmost) <- getBoundaryEntities allInvolvedEntities
    left                  <- addEntityTo leftmost mempty (mempty { width = (blockWidth 10 leftmost L blocks, 0) })
    right                 <- addEntityTo rightmost mempty (mempty { width = (0, blockWidth 10 rightmost R blocks) })
    return (left <> right, (leftmost, rightmost), allInvolvedEntities)
  aux info _ = do
    allInvolvedEntities   <- getEntityNames info
    (leftmost, rightmost) <- getBoundaryEntities allInvolvedEntities
    return (info, (leftmost, rightmost), allInvolvedEntities)

drawBoxTitle :: Font a => Float -> String -> IntermediateT a (Float, Expr)
drawBoxTitle x boxTitle = do
  (txt_width, content) <- linedText (Text {t = [boxTitle]}) (x + 10)
  let txtBoxWidth  = txt_width + 25
      txtBoxHeight = evalHeight content + 15
      txtBox =
        rectangle txtBoxWidth txtBoxHeight x 0.0 0.0 White
          <> horizontalLine x (x + txtBoxWidth)
          <> verticalLine (x + txtBoxWidth) txtBoxHeight
      boxLabel = attachTextToBox txtBox content
  return (txtBoxWidth, boxLabel <> drawSpace 10)
 where
  makeLine x1 x2 y1 y2 = ILine
    [ Attr "x1" <| AttrFloat x1
    , Attr "y1" <| AttrFloat y1
    , Attr "x2" <| AttrFloat x2
    , Attr "y2" <| AttrFloat y2
    , Attr "stroke" <| AttrColor Black
    , Attr "stroke-width" <| AttrFloat 1.5
    ]
  verticalLine x1 h = mempty { blockProgress = \info height -> (0, makeLine x1 x1 (height - h) height) }
  horizontalLine x1 x2 = mempty { blockProgress = \info height -> (0, makeLine x1 x2 height height) }

drawLabeledGroupRequest
  :: Font a => DrawInfo -> ReqBlock -> (LabeledGroupRequest b) -> IntermediateT a (DrawInfo, (Float, Expr), Stmt)
drawLabeledGroupRequest info blocks (LabeledGroupRequest _ altTxt requests) = do
  txt             <- linedText (wrapInBrachets altTxt) 0
  (altInfo, alts) <- drawStmts info blocks requests
  return (altInfo, txt, alts)
 where
  wrapInBrachets Text {..} = Text {t = wrapTxt "[" "]" t}
   where
    wrapTxt s e []       = [s <> e]
    wrapTxt s e [x     ] = [s <> x <> e]
    wrapTxt s e (x : xs) = (s <> x) : wrapTxt "" e xs


composeComponents
  :: Font a => DrawInfo -> Float -> RequestBlock -> (ReqBlock, ReqBlock) -> IntermediateT a (Float, RequestExpr Expr)
composeComponents info yTraslation req@RequestBlock {..} blocks | S <- direction = do
  arrow <- mkArrow reqX arrow_width resX height arrowType direction
  let labelElement = snd label
      propagation  = addEntityFromRequest info from to

  return
    ( evalHeight <| arrow <> labelElement
    , RequestExpr
      direction
      ReqInfo
        { height   = 0
        , arrow_y  = evalHeight <| arrow <> labelElement
        , req_x    = getX from
        , req_y    = yTraslation
        , reqScope = Propagation {top = propagation, bottom = propagation}
        }
      blocks
      Nothing
      labelElement
      (reduceArrow arrowType (reqX + arrow_width) (reqX + arrow_width - resX) Nothing arrow)
      (snd payload)
    )
 where
  reqX        = blockOrientedX from direction
  resX        = blockOrientedX to direction
  height      = max 50 <| 20.0 + evalHeight (snd payload)
  arrow_width = computeWidth label payload

composeComponents info yTraslation req@RequestBlock {..} blocks@(reqBlocks, resBlocks) = do
  let (entityInfo, propagation) = participant
      labelElement              = snd label
      arrow_width               = abs <| reqX - resX
  arrow <- mkArrow reqX 0.0 resX 0.0 arrowType direction
  return
    ( evalHeight labelElement
    , RequestExpr
      direction
      ReqInfo
        { height   = evalHeight <| snd payload
        , arrow_y  = evalHeight labelElement
        , req_x    = getX from
        , req_y    = yTraslation
        , reqScope = propagation
        }
      blocks
      entityInfo
      labelElement
      (reduceArrow arrowType reqX arrow_width entityInfo arrow)
      (snd payload)
    )
 where
  reqX = blockOrientedX from direction
  resX = 2 * getX to - blockOrientedX to direction

computeWidth label payload = 20 + max (fst label) (fst payload)

addEntityFromRequest info (Position req _ _) (Position res _ _) =
  Map.insert req (getEntity info req) . Map.insert res (getEntity info res)

buildRequest
  :: Font a => DrawInfo -> (ReqBlock, ReqBlock) -> Maybe (BasicRequest b) -> IntermediateT a (DrawInfo, Maybe RequestBlock)
buildRequest _ _ Nothing = return (mempty, Nothing)
buildRequest info (reqBlocks, resBlocks) (Just (BasicRequest _ (RequestEntity from _) arrowType (RequestEntity to _) label payload))
  = do
    direction    <- computeDirection from to
    fromPosition <- makeRequestPosition info from reqBlocks
    toPosition   <- makeRequestPosition info to resBlocks
    fromWidth    <- computeEntityWidth label from resBlocks
    toWidth      <- computeEntityWidth label to resBlocks
    let xPosFn = postion fromPosition direction
    rec pInfo@(    pWidth, _) <- addSpaceIfNotEmpty <$> (drawRequestData payload <| xPosFn pWidth)
        labelInfo@(lWidth, _) <- linedText label <| xPosFn lWidth
    let fromWidthByDirection = appendWidth direction (computeWidth labelInfo pInfo) fromWidth
    fromInfo                          <- addEntityTo from mempty (mempty { width = fromWidthByDirection })
    toInfo                            <- addEntityTo to mempty (mempty { width = toWidth })
    (newParticipantInfo, participant) <- addNewParticipant info arrowType fromPosition toPosition direction
    mapM_ addToActiveEntities [from, to]

    return
      ( newParticipantInfo <> fromInfo <> toInfo
      , Just <| RequestBlock
        { from        = fromPosition
        , to          = toPosition
        , payload     = pInfo
        , label       = second (<> drawSpace 5) labelInfo
        , direction   = direction
        , arrowType   = arrowType
        , participant = participant
        }
      )
 where

  addNewParticipant info (NewParticipantArrow _) req r@(Position res _ _) dir = do
    let entity      = getEntity info res
        entityWidth = maybe 0 b_width (box entity) / 2
    participantInfo <- addEntityTo res mempty (mempty { width = (entityWidth, entityWidth) })
    return
      (participantInfo, (Just (res, entity), Propagation {top = Map.delete res, bottom = addEntityFromRequest info req r}))
  addNewParticipant info _ req res _ =
    let propagation = addEntityFromRequest info req res
    in  return (mempty, (Nothing, Propagation {top = propagation, bottom = propagation}))

  addSpaceIfNotEmpty payloadInfo =
    second (\payload -> (if evalHeight payload == 0 then mempty else drawSpace 10) <> payload) payloadInfo

  appendWidth L width (leftWidth, rightWidth) = (width + leftWidth, rightWidth)
  appendWidth _ width (leftWidth, rightWidth) = (leftWidth, width + rightWidth)

  computeEntityWidth l entity block = do
    next <- nextEntity entity
    prev <- prevEntity entity
    let nextLeftWidth  = computeWidth next L block
        prevRightWidth = computeWidth prev R block
    return (prevRightWidth + blockWidth 10 entity L block, nextLeftWidth + blockWidth 10 entity R block)
    where computeWidth entity direction block = maybe 0 (\e -> blockWidth 10 e direction block) entity

  postion r direction w = l + getLabelXPos r direction
   where
    l = case direction of
      L -> -w - 10
      _ -> 10

computeDirection :: Font a => String -> String -> IntermediateT a ArrowDirection
computeDirection req res | req == res = return S
computeDirection req res              = do
  reqMapping <- previousEntities req
  resMapping <- previousEntities res
  return (if null <| resMapping \\ reqMapping then L else R)


mkArrow :: Font a => Float -> Float -> Float -> Float -> Arrows -> ArrowDirection -> IntermediateT a Expr
mkArrow x1 xm x2 v arrow_ty direction = return <| mempty
  { blockProgress =
    \_ height ->
      ( v
      , IGroup
        []
        [ mkLine x1        height       (x1 + xm) height       Black 1.0 arrow_ty
        , mkLine (x1 + xm) height       (x1 + xm) (height + v) Black 1.0 arrow_ty
        , mkLine (x1 + xm) (height + v) x2        (height + v) Black 1.0 arrow_ty
        , IUse [Attr "x" <| AttrFloat x2, Attr "y" <| AttrFloat (height + v)] (print arrow_ty <> show direction)
        ]
      )
  }
 where
  print (FailedArrow _) = "missing"
  print _               = "normal"

  getArrowDraw :: Arrows -> DrawType
  getArrowDraw x = case x of
    Arrow               x -> x
    NewParticipantArrow x -> x
    NewBlockArrow       x -> x
    ClosingBlockArrow   x -> x
    FailedArrow         x -> x

  mkLine x1 y1 x2 y2 color strokeWidth arrow_ty = ILine
    (dashedStroke
      (getArrowDraw arrow_ty)
      [ Attr "x1" <| AttrFloat x1
      , Attr "y1" <| AttrFloat y1
      , Attr "x2" <| AttrFloat x2
      , Attr "y2" <| AttrFloat y2
      , Attr "stroke" <| AttrColor color
      , Attr "stroke-width" <| AttrFloat strokeWidth
      ]
    )
   where
    dashedStroke Dash attrs = Attr "stroke-dasharray" (AttrStr "4 2") : attrs
    dashedStroke _    attrs = attrs


reduceArrow (FailedArrow _) x width _       arrow = reduceArrowLength x width 10 arrow
reduceArrow _               _ _     Nothing arrow = arrow
reduceArrow _ x width (Just (_, info)) arrow =
  let eWidth = maybe 0 b_width <| box info in reduceArrowLength x width (eWidth / 2) arrow

reduceArrowLength :: Float -> Float -> Float -> Expr -> Expr
reduceArrowLength x width reduce arrow
  = let
      (height   , IGroup _ lines     ) = blockProgress arrow Map.empty 0
      (headArrow, IGroup [] tailArrow) = arrowTail [] lines
      scaleTranslation                 = x * (1 - scaleFactor)
      scaleFactor                      = (1 - reduce / width)
    in
      mempty
        { blockProgress =
          \_ h ->
            ( height
            , IGroup
              [Transform <| Translate 0 h]
              [ headArrow
              , IGroup [Transform <| Translate scaleTranslation 0] [IGroup [Transform <| Scale scaleFactor 1] tailArrow]
              ]
            )
        }
 where
  arrowTail acc []                          = (IGroup [] acc, mempty)
  arrowTail acc (x : xs) | [IUse _ _] <- xs = (IGroup [] acc, IGroup [] (x : xs))
  arrowTail acc (x : xs)                    = arrowTail (acc <> [x]) xs


linedText :: Font a => Text -> Float -> IntermediateT a (Float, Expr)
linedText txt@Text {..} x = do
  font <- gets font
  let (width, content) = foldl (compose font width) (0, mempty) t in return (width, content)
 where
  compose font w (width, acc) txt =
    ( max width (infoWidth word)
    , acc
      <> (mempty
           { blockProgress =
             \i h ->
               ( infoHeight word
               , IText
                 [AttrFont <| show font, Attr "font-size" (AttrFloat <| fontSize font)]
                 [ TextSpan
                     [Attr "x" (AttrFloat <| x + txt_x), Attr "y" (AttrFloat <| h + infoHeight word - fontLineGap font)]
                     [TextString txt]
                 ]
               )
           }
         )
    )
   where
    word  = getStringSize font txt
    txt_x = (w - infoWidth word) / 2

