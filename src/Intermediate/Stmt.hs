module Stmt
  ( Stmt(..)
  , StmtBlock(..)
  , ReqInfo(..)
  , RequestExpr(..)
  , StmtInfo(..)
  , Rect(..)
  , expr
  , stmtInfo
  )
where
-- Stmt definition and properties

import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Debug.Trace
import           Flow
import           AbsFlow
import           IntermediateEnv
import           IntermediateAbs

type Stmt = StmtBlock Expr
data RequestExpr a = Empty | RequestExpr ArrowDirection ReqInfo (ReqBlock,ReqBlock) (Maybe (EntityName, EntityInfo)) a a a
data StmtInfo = StmtInfo {
    block :: ReqBlock
  , title ::(Float,Expr)
  }

instance Monoid StmtInfo where
  mempty = StmtInfo {
    block = Map.empty
  , title = (0,mempty)
}

instance Semigroup StmtInfo where
  (<>) a b = StmtInfo {
        block = block a <> block b, title = joinTitle (title a) (title b)
      }
    where
      joinTitle title (0,_) =  title
      joinTitle _ title = title

data Rect = Rect { 
  r_width        :: Float, 
  r_height       :: Float, 
  r_x            :: Float, 
  r_stroke_width :: Float, 
  r_stroke       ::Float, 
  r_color        :: Color }

data StmtBlock a
  = EmptyStmt
  | GenericStmt StmtInfo a
  | DestroyStmt StmtInfo (Set.Set String) (StmtBlock a)
  | RequestStmt StmtInfo a (RequestExpr a) (StmtBlock a) (RequestExpr a) (StmtBlock a)
  | NoSpaceStmt StmtInfo a
  | OptRequest StmtInfo Float Float a [((Float,a),StmtBlock a)]
  | GroupStmt StmtInfo Float Float a Rect (StmtBlock a)
  | NoteStmt StmtInfo a
  | EntityStmt StmtInfo
  | DelayStmt StmtInfo Float Float a
  | TitleStmt StmtInfo

data ReqInfo = ReqInfo {
  height :: Float,
  req_y :: Float,
  req_x :: Float,
  arrow_y :: Float,
  reqScope :: BlockPropagation
}

instance Monoid Stmt where
  mempty = EmptyStmt

instance Semigroup Stmt where
  (<>) EmptyStmt any = any
  (<>) any EmptyStmt = any
  (<>) any (TitleStmt info) = injectInfo mempty any info
  (<>) (TitleStmt info) any = injectInfo info any mempty
  (<>) e@(EntityStmt info) any = GenericStmt (info <> stmtInfo any) (expr e <> line ( drawSpace 20 ) <> expr any)
  (<>) any e@(EntityStmt info) = GenericStmt (stmtInfo any <> info) (expr any <> line ( drawSpace 20 ) <> expr e)
  (<>) (NoSpaceStmt info a) (GenericStmt info' b) = GenericStmt (info <> info') (a <> b)
  (<>) any (NoSpaceStmt info a) = NoSpaceStmt (stmtInfo any <> info) (expr any <> a)
  (<>) (RequestStmt info start req nest res destroy) destroy'@DestroyStmt {} = RequestStmt (info <> stmtInfo destroy) start req nest res (destroy <> destroy')
  (<>) (DestroyStmt info e b) destory@(DestroyStmt info' e' b') | EmptyStmt  <- b = DestroyStmt (info <> info') (Set.union e e') b'
                                                                | otherwise = DestroyStmt (info <> info') e (b <> destory)
  (<>) (DestroyStmt info e b) any = DestroyStmt (info <> stmtInfo any) e (b <> any)
  (<>) any d@(DestroyStmt info _ _) = GenericStmt (stmtInfo any <> info) (expr any <> expr d)
  (<>) any r@(RequestStmt info start req nest res destroy)
      = RequestStmt (stmtInfo any <> stmtInfo r) (expr any <> start <> newParticipants any r) req nest res destroy
  (<>) any any' = GenericStmt (stmtInfo any <> stmtInfo any') (expr any <> newParticipants any any' <> expr any')

newParticipants a b = participants (block <| stmtInfo a) (bottom <| propagation <| expr a) (top <| propagation <| expr b)
 where
  participants :: ReqBlock -> (EntityStore -> EntityStore) -> (EntityStore -> EntityStore) -> Expr
  participants block prev next = mempty
    { blockProgress = \info height ->
                        let scope           = next info Map.\\ prev info
                            newParticipatns = if Map.null scope
                              then mempty
                              else lineWithFilter scope (drawSpace 20) <> lineWithFilter block (drawParticipants scope)
                        in  blockProgress (newParticipatns <> lineWithFilter block (drawSpace 15)) info height
    }

stmtInfo (GenericStmt block _) = block
stmtInfo (DestroyStmt block _ nest) = block <> mempty { title = title } where StmtInfo { title = title } = stmtInfo nest
stmtInfo (RequestStmt block _ _ nest _ destroy) = block <> mempty { title = titleNest } <> mempty { title = titleDestroy }
 where
  StmtInfo { title = titleNest }      = stmtInfo nest
  StmtInfo { title = titleDestroy }   = stmtInfo destroy
stmtInfo (NoSpaceStmt block _     )   = block
stmtInfo (OptRequest block _ _ _ _)   = block
stmtInfo (GroupStmt  block _ _ _ _ _) = block
stmtInfo (NoteStmt block _        )   = block
stmtInfo (TitleStmt block         )   = block
stmtInfo _                            = mempty

injectInfo _    EmptyStmt                         _     = EmptyStmt
injectInfo left (GenericStmt stmtInfo expr      ) right = GenericStmt (left <> stmtInfo <> right) expr
injectInfo left (DestroyStmt stmtInfo store expr) right = DestroyStmt (left <> stmtInfo <> right) store expr
injectInfo left (RequestStmt stmtInfo expr1 req nest res destroy) right =
  RequestStmt (left <> stmtInfo <> right) expr1 req nest res destroy
injectInfo left (NoSpaceStmt stmtInfo expr              ) right     = NoSpaceStmt (left <> stmtInfo <> right) expr
injectInfo left (OptRequest stmtInfo x  width title opts) right     = OptRequest (left <> stmtInfo <> right) x width title opts
injectInfo left (GroupStmt  stmtInfo x1 x2    expr  box stmt) right = GroupStmt (left <> stmtInfo <> right) x1 x2 expr box stmt
injectInfo left (NoteStmt stmtInfo expr                 ) right     = NoteStmt (left <> stmtInfo <> right) expr
injectInfo left (EntityStmt stmtInfo                    ) right     = EntityStmt (left <> stmtInfo <> right)
injectInfo left (DelayStmt stmtInfo x1 x2 expr          ) right     = DelayStmt (left <> stmtInfo <> right) x1 x2 expr
injectInfo left (TitleStmt stmtInfo                     ) right     = TitleStmt (left <> stmtInfo <> right)

-- Stmt -> Expr

expr :: Stmt -> Expr
expr (TitleStmt _)     = mempty
expr EmptyStmt         = mempty
expr (GenericStmt _ a) = a
expr (OptRequest info x width title@Block {..} reqs) =
  let options = buildStackRequest (block <| info) optSpacing title reqs <> (lineWithFilter (block <| info) <| drawSpace 15)
  in  wrapRequestInBox x width options
 where
  buildGroupInfo blocks start ((labelWidth, label), req) =
    lineWithFilter blocks (start <~> translatedLabel) <> startSpacing <> expr req
   where
    translatedLabel = drawSpace 5 <> label <> drawSpace 5
    startSpacing =
      let height = evalHeight <| expr req in if height > 0 then lineWithFilter blocks <| drawSpace 5 else mempty

  buildStackRequest blocks _ start []     = lineWithFilter blocks start
  buildStackRequest blocks _ start [info] = buildGroupInfo blocks start info
  buildStackRequest blocks separator start (y : ys) =
    let req  = buildGroupInfo blocks start y
        reqs = buildStackRequest blocks separator mempty ys
    in  req <> separator <> reqs

  optSpacing = lineWithFilter (block <| info) (drawSpace 15 <> drawDashLine x (x + width))

  wrapRequestInBox :: Float -> Float -> Expr -> Expr
  wrapRequestInBox x width blockReq = blockReq <~> rectangle width (evalHeight blockReq) x 0.0 1.5 Black

expr (GroupStmt info startLineX endLineX title Rect{..} body) =
  lineWithFilter groupBlock (titleBox <> dash_line <> lineSpace) <> expr body <> lineWithFilter groupBlock (lineSpace <> dash_line)
 where
  titleBox = attachTextToBox (rectangle r_width (evalHeight title + 10) r_x r_stroke_width r_stroke r_color) title
  groupBlock = block <| info
  lineSpace  = drawSpace 20
  dash_line  = drawDashLine startLineX endLineX
expr (EntityStmt _) = mempty { blockProgress = \info height -> blockProgress (drawParticipants info) info height }
expr (DelayStmt StmtInfo {..} x1 x2 a) = rectangle (abs <| x1 - x2) totalHeight x1 1.0 1.0 White <~> delayExpr
 where
  totalHeight = evalHeight delayExpr
  delayExpr =
    (dashedBlock 30 block <~> dashLineWithFilter block (drawSpace 30))
      <> drawSpace 20
      <> a
      <> drawSpace 20
      <> (dashedBlock 30 block <~> dashLineWithFilter block (drawSpace 30))
expr (NoteStmt info a) = lineWithFilter (block <| info) a
expr (DestroyStmt info entities a)
  = (Block
      { propagation   = mempty { bottom = \t -> Set.foldl (flip Map.delete) t entities }
      , blockProgress = \info height ->
        ( destroyLineHeight
        , Set.foldl
          (\acc entity ->
            let x_pos = x <| info Map.! entity
            in  acc <> dashLine height (height + destroyLineHeight) x_pos <> addX x_pos (height + destroyLineHeight / 2)
          )
          mempty
          entities
        )
      }
    )
    <> translate 0 (-destroyLineHeight) (lineWithFilter (block info) (drawSpace destroyLineHeight))
    <> expr a
 where
  destroyLineHeight = 40
  addX x height = IUse [Attr "x" <| AttrFloat x, Attr "y" <| AttrFloat height] "x"

expr (NoSpaceStmt _ a                        ) = a
expr (RequestStmt _ start req _ Empty destroy) = start <> printSingleReq lineWithFilter destroy req
expr (RequestStmt _ start req@(RequestExpr _ _ (_, resBlocks) _ label _ _) nest res destroy) =
  let leftSpacing   = lineWithFilter resBlocks <| drawSpace 20
      rightSpaceing = lineWithFilter resBlocks <| drawSpace (20 + evalHeight label)
      nesting       = leftSpacing <> expr nest <> rightSpaceing
  in  start <> (block req res nesting <~> (singleRequestExpr req lineWithFilter <> nesting)) <> attachDestroy res destroy
 where

  attachDestroy res@(RequestExpr S _ _ _ _ _ _) destroy = singleRequestExpr res (\_ b -> b) <> expr destroy
  attachDestroy res                             destroy = printSingleReq (\_ b -> b) destroy res

  block :: RequestExpr Expr -> RequestExpr Expr -> Expr -> Expr
  block _ Empty _ = mempty
  block (RequestExpr _ reqInfo _ _ _ _ _) (RequestExpr _ resInfo _ _ _ arrow _) block =
    let startX         = req_x resInfo - 10
        reqBlockHeight = evalHeight arrow + height reqInfo + evalHeight block
    in  translate 0 (arrow_y reqInfo) <| mempty
          { blockProgress = \_ height ->
                              ( reqBlockHeight
                              , boxSvg startX height reqBlockHeight 20.0 "white" 1.0 Black 0
                                <> boxSvg startX height reqBlockHeight 20.0 "url(#pinstripe)" 0.5 Black 1
                              )
          }
   where
    boxSvg :: Float -> Float -> Float -> Float -> String -> Float -> Color -> Float -> Element
    boxSvg x y h w fill opacity stroke stroke_width = IRect
      [ Attr "x" <| AttrFloat x
      , Attr "y" <| AttrFloat y
      , Attr "width" <| AttrFloat w
      , Attr "height" <| AttrFloat h
      , Attr "stroke" <| AttrColor stroke
      , Attr "fill-opacity" <| AttrFloat opacity
      , Attr "stroke-width" <| AttrFloat stroke_width
      , Attr "fill" <| AttrStr fill
      ]
  block _ _ _ = error "expr.block not suppored match" 
expr _ =  error "expr not suppored match" 

printSingleReq :: (ReqBlock -> Expr -> Expr) -> Stmt -> RequestExpr Expr -> Expr
printSingleReq lineFn destroy   r@(RequestExpr S _ _ _ _ _ _) = singleRequestExpr r lineFn <> expr destroy
printSingleReq lineFn EmptyStmt req                           = singleRequestExpr req lineFn
printSingleReq lineFn destroy (RequestExpr direction reqInfo nestingBlocks participant label arrow payload) =
  singleRequestExpr noPayloadRequest lineFn <> wrapInLine (entities destroy) payload (expr destroy)
 where

  wrapInLine _ payload destroy | evalHeight payload == 0 = destroy
  wrapInLine block payload destroy =
    payload
      <~> (  destroy
          <> (lineWithFilter (Set.foldl (\acc entity -> Map.insert entity [] acc) mempty block) <| drawSpace lineHeight)
          )
   where
    lineHeight    = abs (payloadHeight - destroyHeight)
    payloadHeight = evalHeight payload
    destroyHeight = evalHeight <| destroy

  noPayloadRequest = RequestExpr direction reqInfo nestingBlocks participant label arrow mempty

  entities (DestroyStmt _ es _) = es
  entities _                    = mempty
printSingleReq _ _ _ = error "printSingleReq not suppored match"  


singleRequestExpr :: RequestExpr Expr -> (ReqBlock -> Expr -> Expr) -> Expr
singleRequestExpr Empty _ = mempty
singleRequestExpr (RequestExpr direction ReqInfo {..} blocks@(reqBlocks, _) participant label arrow payload) lineFn =
  translate
    0
    req_y
    (  lineFn reqBlocks (labelSpacingX participant <> label)
    <> (mempty { propagation = reqScope })
    <> lineWithFilter (makeBlocks direction blocks) (participantExpr participant <~> arrow <~> payload)
    )
 where

  labelSpacingX :: Maybe (EntityName, EntityInfo) -> Expr
  labelSpacingX Nothing           = mempty
  labelSpacingX (Just (_, eInfo)) = drawSpace <| (maybe 0 b_height <| box eInfo) / 2

  participantExpr :: Maybe (EntityName, EntityInfo) -> Expr
  participantExpr Nothing              = mempty
  participantExpr (Just (name, eInfo)) = translate 0 (-(maybe 0 b_height <| box eInfo) / 2) <| drawParticipant name eInfo

  makeBlocks S (reqBlocks, _       ) = reqBlocks
  makeBlocks _ (_        , resBlock) = resBlock

drawDashLine :: Float -> Float -> Expr
drawDashLine x1 x2 = mempty
  { blockProgress = \_ s ->
                      ( 1
                      , ILine
                        [ Attr "x1" <| AttrFloat x1
                        , Attr "y1" <| AttrFloat s
                        , Attr "x2" <| AttrFloat x2
                        , Attr "y2" <| AttrFloat s
                        , Attr "stroke" <| AttrColor Black
                        , Attr "stroke-dasharray" <| AttrStr "4 2"
                        , Attr "stroke-width" <| AttrFloat 1.0
                        ]
                      )
  }

drawParticipants :: EntityStore -> Expr
drawParticipants = Map.foldlWithKey (\acc k e -> acc <~> drawParticipant k e) mempty

drawParticipant :: EntityName -> EntityInfo -> Expr
drawParticipant name info@EntityInfo { box = box, x = x_pos, txt = Just blockText } =
  let width   = maybe 0 b_width box
      box_pos = x_pos - width / 2
      height  = maybe 0 b_height box
  in  attachTextToBox (rectangle width height box_pos 1.0 1.0 Black)
                      (translate x_pos 0 blockText) { propagation = mempty { bottom = Map.insert name info } }
drawParticipant _ _ = error "drawParticipant not supported match"

line :: Expr -> Expr
line = lineWithFilter Map.empty

lineWithFilter :: Map.Map String b -> Expr -> Expr
lineWithFilter diff stmtBlock = addLine (evalHeight stmtBlock) diff <~> stmtBlock

dashLineWithFilter :: Map.Map String b -> Expr -> Expr
dashLineWithFilter diff stmtBlock = drawLine dashLine (evalHeight stmtBlock) diff <~> stmtBlock

addLine :: Float -> Map.Map String b -> Expr
addLine height _ | height <= 0 = mempty
addLine height diff            = drawLine line height diff
 where
  line y1 y2 x = ILine
    [ Attr "x1" <| AttrFloat x
    , Attr "y1" <| AttrFloat y1
    , Attr "x2" <| AttrFloat x
    , Attr "y2" <| AttrFloat y2
    , Attr "style" <| AttrStr "stroke:rgb(0,0,0);stroke-width:1"
    ]

dashedBlock height block = mempty
  { blockProgress = \i h ->
                      ( height
                      , Map.foldlWithKey (\acc key elem -> acc <> drawBlockComposition h (h + height) (i Map.! key) elem)
                                         mempty
                                         block
                      )
  }
 where
  drawBlockComposition y1 y2 entity = snd . foldr (aux y1 y2) (x entity, mempty)
  aux y1 y2 dir (x, elem) = let newX = x + move dir in (newX, elem <> drawDashlock newX y1 y2)
  move = maybe 0 (\dir -> if dir == L then 10 else -10)

drawLine line height diff = mempty
  { blockProgress = \i h -> (height, foldl (\acc entity -> acc <> line h (h + height) (x entity)) mempty (i Map.\\ diff))
  }

drawDashlock x y1 y2 = rect 20 height (x - 10) 1.0 "white" 1.0 White <> dashLine y1 y2 (x - 10) <> dashLine y1 y2 (x + 10)
 where
  height = abs <| y1 - y2
  rect box_width height x fill_opacity fill strokeWidth color = IRect
    [ Attr "x" <| AttrFloat x
    , Attr "y" <| AttrFloat y1
    , Attr "rx" <| AttrInt 1
    , Attr "ry" <| AttrInt 1
    , Attr "width" <| AttrFloat box_width
    , Attr "height" <| AttrFloat height
    , Attr "stroke" <| AttrColor color
    , Attr "stroke-width" <| AttrFloat strokeWidth
    , Attr "fill-opacity" <| AttrFloat fill_opacity
    , Attr "fill" <| AttrStr fill
    ]

dashLine y1 y2 x = ILine
  [ Attr "x1" <| AttrFloat x
  , Attr "y1" <| AttrFloat y1
  , Attr "x2" <| AttrFloat x
  , Attr "y2" <| AttrFloat y2
  , Attr "style" <| AttrStr "stroke:rgb(0,0,0);stroke-width:1"
  , Attr "stroke-dasharray" (AttrStr "4 2")
  ]
-- end Stmt


