module IntermediateAbs where

import           Data.Map
import           Flow
import           Font

newtype IntermediateInfo a = DrawInfo
  { entityInfo :: a
  } deriving (Show)


data Transform
  = Scale Float Float
  | Rotate Float
  | Translate Float
              Float
  deriving (Show)

data AttrValue
  = AttrInt Int
  | AttrFloat Float
  | AttrStr String
  | AttrColor Color
  deriving (Show)

data Color
  = Black
  | White

instance Show Color where
  show White = "white"
  show Black = "black"

data Attr
  = Attr String
         AttrValue
  | List String
         [AttrValue]
  | Transform Transform
  | AttrFont String
  | Type String
  deriving (Show)

data Draw =
  Draw [Attr]
       [Element]
  deriving (Show)

data TextContent
  = TextString String
  | TextSpan [Attr] [TextContent]
  deriving (Show)


type FontFamily = String
type Src = String
type Format = String

data StyleElement = FontFace FontFamily Src Format
  deriving (Show)

data Element
  = IRect [Attr]
  | IPath [Attr]
  | IStyle [Attr] [StyleElement]
  | ILine [Attr]
  | IGroup [Attr]
           [Element]
  | IUse [Attr]
         String
  | ITitle [Attr]
           String
  | IPattern [Attr]
             [Element]
  | Defs [Element]
  | IText [Attr]
          [TextContent]
  deriving (Show)

instance Monoid Element where
  mempty = IGroup [] []

instance Semigroup Element where
  (<>) a (IGroup [] []) = a
  (<>) (IGroup [] []) b = b
  (<>) (IGroup [] a) (IGroup [] b) = IGroup [] (a <> b)
  (<>) a b = IGroup [] [a, b]
