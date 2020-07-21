module AbsSvg where

type SvgIdentifier = String

type Extension = String

data AttrValue
  = AttrInt Int
  | AttrFloat Float
  | AttrStr String
  deriving (Show)

data Transform
  = Scale Float Float
  | Rotate Float
  | Translate Float
              Float
  deriving (Show)

data Attr
  = Attr String
         AttrValue
  | AttrList String
             [AttrValue]
  | AttrTransform Transform
  | AttrFont String
  | Type String
  deriving (Show)

data SVG =
  SVG [Attr]
      [SvgElement]
  deriving (Show)

data SvgElement =
  SvgElement [Attr]
             Element
  deriving (Show)

data TextContent
  = TextString String
  | TextSpan TSpan
  deriving (Show)

data TSpan =
  TSpan [Attr]
        [TextContent]
  deriving (Show)

type FontFamily = String
type Src = String
type Format = String

data StyleElement = FontFace FontFamily Src Format
  deriving (Show)

data Element
  = Rect
  | Path
  | Line
  | Style [StyleElement]
  | SvgGroup [SvgElement]
  | Use SvgIdentifier
  | Title String
  | Desc String
  | Pattern [SvgElement]
  | Defs [SvgElement]
  | SvgText [TextContent]
  | SvgForeingElement [ForeingElement]
  deriving (Show)

data ForeingElement
  = Div Extension
        [Attr]
        [ForeingElement]
  | Span Extension
         [Attr]
         [ForeingElement]
  | Text String
  | Pre Extension
        [Attr]
        [ForeingElement]
  deriving (Show)

instance Monoid SVG where
  mempty = SVG [] []
  mappend = (<>)

instance Semigroup SVG where
  (<>) (SVG attrs elms) (SVG attrs' elms') = SVG (attrs ++ attrs') (elms ++ elms')
