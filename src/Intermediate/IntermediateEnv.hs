
module IntermediateEnv where

import           AbsFlow                       as Abs
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Maybe
import           Debug.Trace
import           Flow
import           Font
import           IntermediateAbs
import           Control.Arrow
import           Control.Category
import           Prelude                 hiding ( id
                                                , (.)
                                                )

data ArrowDirection
  = L
  | R
  | S
  deriving (Eq)

reverseDirection L = R
reverseDirection S = S
reverseDirection _ = L

instance Show ArrowDirection where
  show L = "ArrowLeft"
  show R = "ArrowRight"
  show S = "ArrowSelf"

type ReqBlock = Map.Map String [Maybe ArrowDirection]

data Box = Box
  { b_height :: Float
  , b_width :: Float
  } deriving (Show, Eq, Ord, Read)

instance Semigroup Box where
  (<>) a b =
    Box {b_height = max (b_height a) (b_height b), b_width = max (b_width a) (b_width b)}

instance Monoid Box where
  mempty = Box {b_height = 0, b_width = 0}

data EntityInfo = EntityInfo
  { width :: (Float,Float)
  , x :: Float
  , box :: Maybe Box
  , txt :: Maybe Expr
  }

instance Show EntityInfo where
  show EntityInfo{..} = show (x, width)

instance Semigroup EntityInfo where
  (<>) a b =
    EntityInfo
      { width =  joinWidth (width a) (width b)
      , x = x a + x b
      , box = box a <> box b
      , txt = txt a <> txt b
      }
    where
      joinWidth (wla,wra) (wlb,wrb) = (max wla wlb, max wra wrb)

instance Monoid EntityInfo where
  mempty = EntityInfo {width = (0,0), x = 0, txt = mempty, box = Nothing}

type IntermediateT a  = ExceptT String (State (Env a))
type Expr = Block EntityStore
type BlockPropagation = Propagation EntityStore
type EntityStore = Map.Map EntityName EntityInfo

data Env a =
   Env
     { entityDefs :: Map.Map String EntityDef
     , font :: FontTable a
     , scope :: Set.Set EntityName
     , boxWidth :: Float
     , boxHeight :: Float
     }

initEnv entites font =
  Env {entityDefs = entitiesMap entites, font = font, boxWidth = 60.0, boxHeight = 60.0, scope = Set.empty}

data Propagation a = Propagation {
    top :: a -> a
  , bottom :: a -> a
}

instance Monoid BlockPropagation where
  mempty = Propagation {top=id, bottom=id}

instance Semigroup BlockPropagation where
  (<>) a b =
    Propagation
      { top = top_a . top_b
      , bottom = bottom_b . bottom_a
      }
    where
      Propagation{top=top_a, bottom=bottom_a} = a
      Propagation{top=top_b, bottom=bottom_b} = b

data Block a =
  Block { propagation :: Propagation a
        , blockProgress :: a -> Float -> (Float,Element)
        }

class BuildBlock a where
  (<~>) :: a -> a -> a
  translate :: Float -> Float -> a -> a
  scale :: Float -> Float -> a -> a

instance Monoid Expr where
  mempty = Block { propagation=mempty, blockProgress = \_ _ -> (0,IGroup [] []) }

instance Semigroup Expr where
  (<>) a b =
    Block
      { propagation= prop_a <> prop_b
      , blockProgress = \info height -> let (height_a,elems_a) = progress_a (top_a info) height
                                            (height_b,elems_b) = progress_b (top_b <| bottom_a info) (height + height_a)
                                        in (height_a + height_b,elems_a <> elems_b)
      }
    where
      Block{propagation=prop_a@Propagation{top=top_a, bottom=bottom_a}, blockProgress=progress_a} = a
      Block{propagation=prop_b@Propagation{top=top_b, bottom=bottom_b}, blockProgress=progress_b} = b

instance BuildBlock Expr where
  translate x y expr = expr
    { blockProgress = \info height -> let (height_a,elems_a) = blockProgress expr info height
                                      in (height_a+y, IGroup [Transform <| Translate x y] [elems_a])
    }
  scale x y expr = expr
    { blockProgress = \info height -> let (height_a,elems_a) = blockProgress expr info height
                                      in (height_a*y, IGroup [Transform <| Scale x y] [elems_a])
    }
  (<~>) a b =
    Block
      { propagation= prop_a <> prop_b
      , blockProgress = \info height -> let (height_a,elems_a) =  blockProgress a (top_a info) height
                                            (height_b,elems_b) =  blockProgress b  (top_b info) height
                                        in (max height_a height_b,elems_a <> elems_b)
      }
    where
      Block{propagation=prop_a@Propagation{top=top_a}, blockProgress=progress_a} = a
      Block{propagation=prop_b@Propagation{top=top_b}, blockProgress=progress_b} = b

evalHeight Block {..} = fst <| blockProgress Map.empty 0

intToFloat :: Int -> Float
intToFloat = fromInteger . toInteger

getPreviousEntitiesAndItself :: [String] -> IntermediateT a [(EntityName, [EntityName])]
getPreviousEntitiesAndItself entities =
  sortOn (length . snd) <$> mapM (\e -> (e, ) . prevs . (Map.! e) <$> gets entityDefs) entities

getLeftSideEntity :: String -> IntermediateT a EntityName
getLeftSideEntity e = last . prevs . (Map.! e) <$> gets entityDefs

getSortedEntities :: IntermediateT a [String]
getSortedEntities = map fst . sortOn (length . prevs . snd) . Map.assocs <$> gets entityDefs

previousEntities :: String -> IntermediateT a [String]
previousEntities entity = prevs . (Map.! entity) <$> gets entityDefs

prevEntity :: Font a => String -> IntermediateT a (Maybe String)
prevEntity entity = do
  previousEntities <- previousEntities entity
  return <| if null previousEntities then Nothing else Just <| last previousEntities

nextEntity :: Font a => String -> IntermediateT a (Maybe String)
nextEntity entity = do
  allEntities      <- getSortedEntities
  previousEntities <- previousEntities entity
  let nextEntities = allEntities \\ (previousEntities <> [entity])
  return <| if null nextEntities then Nothing else Just <| head nextEntities

--------------------------------------------------------------------------
------------ TODO create utils file with common function used ------------
--------------------------------------------------------------------------

rectangle box_width height x fill_opacity strokeWidth color = mempty
  { blockProgress = \_ y ->
                      ( height
                      , IRect
                        [ Attr "x" <| AttrFloat x
                        , Attr "y" <| AttrFloat y
                        , Attr "rx" <| AttrInt 1
                        , Attr "ry" <| AttrInt 1
                        , Attr "width" <| AttrFloat box_width
                        , Attr "height" <| AttrFloat height
                        , Attr "fill" <| AttrColor White
                        , Attr "stroke" <| AttrColor color
                        , Attr "stroke-width" <| AttrFloat strokeWidth
                        , Attr "fill-opacity" <| AttrFloat fill_opacity
                        ]
                      )
  }

blurRectangle box_width height x = mempty
  { blockProgress = \_ y ->
                      ( height
                      , IRect
                        [ Attr "x" <| AttrFloat x
                        , Attr "y" <| AttrFloat y
                        , Attr "rx" <| AttrInt 1
                        , Attr "ry" <| AttrInt 1
                        , Attr "width" <| AttrFloat box_width
                        , Attr "height" <| AttrFloat height
                        , Attr "stroke-width" <| AttrFloat 3
                        , Attr "fill" <| AttrColor White
                        , Attr "filter" <| AttrStr "url(#f1)"
                        ]
                      )
  }

drawSpace :: Float -> Expr
drawSpace y = mempty { blockProgress = \_ _ -> (y, IGroup [] []) }

drawSpace_20 = drawSpace 20


attachTextToBox :: Expr -> Expr -> Expr
attachTextToBox box text = box <~> translate 0 ((evalHeight box - evalHeight text) / 2) text
