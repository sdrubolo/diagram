module Svg
  ( tranformTo
  )
where

import           AbsSvg
import           IntermediateAbs               as IAbs
import           IntermediateTranform

instance TranformTo Draw SVG where
  tranformTo (Draw attrs elements) =
    SVG (tranformTo attrs <> svgAttrs) (tranformTo elements)
    where
      svgAttrs =
        [ AbsSvg.Attr "xmlns" (AbsSvg.AttrStr "http://www.w3.org/2000/svg")
        , AbsSvg.Attr "xmlns:xhtml" (AbsSvg.AttrStr "http://www.w3.org/1999/xhtml")
        , AbsSvg.Attr "xmlns:xlink" (AbsSvg.AttrStr "http://www.w3.org/1999/xlink")
        , AbsSvg.Attr "version" (AbsSvg.AttrStr "1.1")
        , AbsSvg.Attr "xmlns:flow-diagram" (AbsSvg.AttrStr "root")
        , AbsSvg.Attr "preserveAspectRatio" (AbsSvg.AttrStr "xMinYMin")
        , AbsSvg.Attr "style" (AbsSvg.AttrStr "background-color:white")
        ]

instance TranformTo IAbs.Attr AbsSvg.Attr where
  tranformTo (IAbs.Attr name value) = AbsSvg.Attr name (tranformTo value)
  tranformTo (IAbs.List name values) = AbsSvg.AttrList name (map tranformTo values)
  tranformTo (IAbs.Transform transform) = AbsSvg.AttrTransform (tranformTo transform)
  tranformTo (IAbs.AttrFont font) = AbsSvg.AttrFont font
  tranformTo (IAbs.Type typeAttr) = AbsSvg.Type typeAttr

instance TranformTo IAbs.Transform AbsSvg.Transform where
  tranformTo (IAbs.Scale x y) = AbsSvg.Scale x y
  tranformTo (IAbs.Rotate value) = AbsSvg.Rotate value
  tranformTo (IAbs.Translate x y) = AbsSvg.Translate x y

instance TranformTo IAbs.AttrValue AbsSvg.AttrValue where
  tranformTo (IAbs.AttrInt value) = AbsSvg.AttrInt value
  tranformTo (IAbs.AttrFloat value) = AbsSvg.AttrFloat value
  tranformTo (IAbs.AttrStr value) = AbsSvg.AttrStr value
  tranformTo (IAbs.AttrColor value) = AbsSvg.AttrStr (show value)

instance (TranformTo a b) => TranformTo [a] [b] where
  tranformTo = map tranformTo

instance TranformTo IAbs.TextContent AbsSvg.TextContent where
  tranformTo (IAbs.TextString value) = AbsSvg.TextString value
  tranformTo (IAbs.TextSpan attrs value) = AbsSvg.TextSpan (AbsSvg.TSpan (tranformTo attrs) (tranformTo value))

instance TranformTo IAbs.Element SvgElement where
  tranformTo (IRect attrs) = SvgElement (tranformTo attrs) Rect
  tranformTo (IPath attrs) = SvgElement (tranformTo attrs) Path
  tranformTo (ILine attrs) = SvgElement (tranformTo attrs) Line
  tranformTo (IGroup attrs elements) =
    SvgElement (tranformTo attrs) (SvgGroup (tranformTo elements))
  tranformTo (IUse attrs identifier) = SvgElement (tranformTo attrs) (Use identifier)
  tranformTo (ITitle attrs title) = SvgElement (tranformTo attrs) (Title title)
  tranformTo (IPattern attrs elements) =
    SvgElement (tranformTo attrs) (Pattern (tranformTo elements))
  tranformTo (IAbs.Defs elements) = SvgElement [] (AbsSvg.Defs (tranformTo elements))
  tranformTo (IText attrs context) =
    SvgElement (tranformTo attrs) (SvgText (tranformTo context))
  tranformTo (IAbs.IStyle attrs value) = SvgElement (tranformTo attrs) (Style (tranformTo value))

instance TranformTo IAbs.StyleElement AbsSvg.StyleElement where
  tranformTo (IAbs.FontFace fontFamily src format) = AbsSvg.FontFace fontFamily src format
