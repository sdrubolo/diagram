module PrintSvg where

import           AbsSvg
import           Data.Char
import           Print

printSvg :: (Print a) => a -> String
printSvg = prettyPrint

newLine = justShow 0 "\n"

instance Print SVG where
  prt i (SVG attributes elements) =
    concatD
      [ doc (showString "<svg ")
      , prt i attributes
      , doc (showString ">")
      , prt (i + 1) elements
      , doc (showString "</svg>")
      ]

instance Print Int where
  prt _ x = doc (shows x)

instance Print TSpan where
  prt i (TSpan attributes txt) =
    concatD
      [ justShow i "<tspan"
      , prt i attributes
      , doc (showString ">")
      , prt 0 txt
      , justShow i "</tspan>"
      , newLine
      ]

instance Print TextContent where
  prt i (TextString s) = justShow 0 s
  prt i (TextSpan s) = prt i s

instance Print StyleElement where
  prt i (FontFace fontFamily src format) = concatD [ justShow i "@font-face { " ,
                                                     justShow i "font-family : ", prt 0 fontFamily, justShow i ";",
                                                     justShow i "src : url(", prt 0 src, justShow i ") ",
                                                     justShow i "format(", prt 0 format, justShow i ");",
                                                     justShow i "}"]

instance Print SvgElement where
  prt i (SvgElement attributes element) =
    case element of
      Style value -> concatD [ justShow i "<style>" , prt 0 value , justShow i "</style>" ]
      Rect ->
        concatD [justShow i "<rect", prt i attributes, doc (showString "/>"), newLine]
      Path ->
        concatD [justShow i "<path", prt i attributes, doc (showString "/>"), newLine]
      Line ->
        concatD [justShow i "<line", prt i attributes, doc (showString "/>"), newLine]
      SvgGroup elements ->
        concatD
          [ justShow i "<g"
          , prt i attributes
          , doc (showString ">")
          , prt (i + 1) elements
          , justShow i "</g>"
          , newLine
          ]
      Use identifier ->
        concatD
          [ justShow i "<use"
          , prt i attributes
          , doc (showString " xlink:href=\"#")
          , justShow 0 identifier
          , doc (showString "\"")
          , justShow 0 "/>"
          , newLine
          ]
      SvgText text ->
        concatD
          [ justShow i "<text"
          , prt i attributes
          , doc (showString ">")
          , prt i text
          , justShow i "</text>"
          , newLine
          ]
      Title title ->
        concatD [justShow i "<title>", justShow 0 title, justShow i "</title>", newLine]
      Desc desc ->
        concatD [justShow i "<desc>", prt i desc, justShow i "</desc>", newLine]
      Defs elements ->
        concatD [justShow i "<defs>", prt (i + 1) elements, justShow i "</defs>", newLine]
      Pattern elements ->
        concatD
          [ justShow i "<pattern"
          , prt i attributes
          , doc (showString ">")
          , prt (i + 1) elements
          , justShow i "</pattern>"
          , newLine
          ]
      SvgForeingElement foreingElement ->
        concatD
          [ justShow i "<foreignObject"
          , prt i attributes
          , doc (showString ">")
          , prt (i + 1) foreingElement
          , justShow i "</foreignObject>"
          , newLine
          ]

instance Print Attr where
  prt i (Attr attName attrValue) =
    concatD
      [ justShow 0 " "
      , justShow 0 attName
      , doc (showString "=\"")
      , prt 0 attrValue
      , doc (showString "\"")
      ]
  prt i (AttrList attName attrValue) =
    concatD
      [ justShow 0 " "
      , justShow 0 attName
      , doc (showString "=\"")
      , prt i attrValue
      , doc (showString "\"")
      ]
  prt i (AttrTransform transform) =
    concatD
      [ justShow 0 " "
      , justShow 0 "transform"
      , doc (showString "=\"")
      , prt i transform
      , doc (showString "\"")
      ]
  prt i (AttrFont font) -- TODO need to introduce style attribute which takes font-family
   =
    concatD
      [ justShow 0 " "
      , justShow 0 "style"
      , doc (showString "=\"")
      , doc (showString "font-family:")
      , doc (showString font)
      , doc (showString "\"")
      ]

instance Print Transform where
  prt i (Scale x y) = concatD [justShow 0 "scale(", prt i x, doc (showString ","), prt i y, doc (showString ")")]
  prt i (Rotate value) = concatD [justShow 0 "rotate(", prt i value, doc (showString ")")]
  prt i (Translate x y) =
    concatD
      [ justShow 0 "translate("
      , prt i x
      , doc (showString ",")
      , prt i y
      , doc (showString ")")
      ]

instance Print AttrValue where
  prt i (AttrStr value) = doc $ showString value
  prt i (AttrInt value) = doc $ showString (show value)
  prt i (AttrFloat value) = doc $ showString (show value)

instance Print ForeingElement where
  prt i (Div ext attr elements) =
    concatD
      [ justShow 0 $ "<" ++ ext ++ ":div"
      , prt 0 attr
      , justShow 0 ">"
      , prt (i + 1) elements
      , justShow 0 $ "</" ++ ext ++ ":div>"
      ]
  prt i (Span ext attr elements) =
    concatD
      [ justShow 0 $ "<" ++ ext ++ ":span"
      , prt 0 attr
      , justShow 0 ">"
      , prt (i + 1) elements
      , justShow 0 $ "</" ++ ext ++ ":span>"
      ]
  prt i (Pre ext attr elements) =
    concatD
      [ justShow 0 $ "\n<" ++ ext ++ ":pre"
      , prt 0 attr
      , justShow 0 ">"
      , prt (i + 1) elements
      , justShow 0 $ "</" ++ ext ++ ":pre>"
      , newLine
      ]
  prt _ (Text text) = concatD [justShow 0 text]
