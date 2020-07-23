module IntermediateData
  ( drawRequestData
  )
where

import           AbsData
import           Control.Monad.State.Lazy
import           Data.Foldable
import           Data.Maybe
import           Debug.Trace
import           Flow
import           Font
import           IntermediateAbs
import           IntermediateEnv
import           Data.Bifunctor
import           Data.Char

type Height = Float
type Width = Float

data ObjectScope = ObjectScope {
  identation :: Float,
  startSymbol :: String,
  endSymbol :: String,
  x1 :: Float,
  x2 :: Float,
  y :: Float
}

mapAccumLM :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM _ a []       = return (a, [])
mapAccumLM f a (x : xs) = do
  (b, c ) <- f a x
  (d, cs) <- mapAccumLM f b xs
  return (d, c : cs)

boolToString :: Bool -> String
boolToString = map toLower . show

drawRequestData :: Font a => Maybe RequestData -> Float -> IntermediateT a (Float, Expr)
drawRequestData Nothing  _ = return (0, mempty)
drawRequestData (Just v) x = do
  ((height, width), payload) <- drawData v
  currentFont                <- gets font
  return
    ( width
    , mempty
      { blockProgress = \i h ->
                          ( height
                          , IGroup
                            [Transform <| Translate x (h + 9)]
                            [ IText
                                [ AttrFont <| show currentFont
                                , Attr "font-size" (AttrFloat <| fontSize currentFont)
                                , Attr "word-spacing" <| AttrFloat (-7.0)
                                ]
                                payload
                            ]
                          )
      }
    )

drawData :: Font a => RequestData -> IntermediateT a ((Height, Width), [TextContent])
drawData (Json json       ) = second (: []) <$> drawJsonObj 0 0 0 json ""
drawData (Form assignments) = mapAccumLM drawAssignments (0, 0) assignments
 where
  drawAssignments (height, width) assignment = do
    ((assignHeight, assignWidth), assign) <- drawAssignment height assignment
    return ((assignHeight + height, max width assignWidth), assign)

drawAssignment :: Font a => Float -> Assignment -> IntermediateT a ((Height, Width), TextContent)
drawAssignment currentHeight assignment = do
  currentFont <- gets font
  let (left, right) = case assignment of
        AssignmentById (Identifier id1) (Identifier id2) -> (id1, id2)
        AssignmentByValue (Identifier id1) righthand ->
          let right = case righthand of
                StringValue  s -> show s
                BoolValue    b -> boolToString b
                IntegerValue i -> show i
          in  (id1, right)
  let assignmentSize = getStringSize currentFont (left <> equalSymb <> right <> sepSymb)
  return ((infoHeight assignmentSize, infoWidth assignmentSize), element left right)
 where
  equalSymb = "="
  sepSymb   = ";"
  element l r = TextSpan [Attr "y" (AttrFloat currentHeight), Attr "x" (AttrFloat 0)]
                         [TextString l, TextString equalSymb, TextString r, TextString sepSymb]

drawJsonObj :: Font a => Float -> Float -> Float -> JsonObj -> String -> IntermediateT a ((Height, Width), TextContent)
drawJsonObj x1 x2 y obj separator = do
  currentFont <- gets font
  let spaceWidth = getCharWidth currentFont ' '
  case obj of
    (JsonObjObject pairs) ->
      let coords = ObjectScope {identation = spaceWidth, x1 = x1, x2 = x2, y = y, startSymbol = "{", endSymbol = "}"}
      in  drawItemList coords pairs drawJsonPair
    (JsonObjArray array) ->
      let coords = ObjectScope {identation = spaceWidth * 2, x1 = x1, x2 = x2, y = y, startSymbol = "[", endSymbol = "]"}
      in  drawItemList coords array drawJsonValue
 where
  drawItemList
    :: Font a
    => ObjectScope
    -> [b]
    -> (ObjectScope -> String -> b -> IntermediateT a ((Height, Width), TextContent))
    -> IntermediateT a ((Height, Width), TextContent)
  drawItemList coords@ObjectScope {..} ls fun = do
    currentFont <- gets font
    let separatorCharSize = getStringSize currentFont startSymbol
        bracketWidth      = infoWidth separatorCharSize
        bracketHeight     = infoHeight separatorCharSize
        endSymbolSep      = endSymbol <> separator
        bracketHeightSep  = infoHeight separatorCharSize
    case ls of
      [] -> return
        ( (bracketHeightSep, bracketWidth * 2)
        , TextSpan
          []
          [ TextSpan [Attr "x" <| AttrFloat x1]                   [TextString startSymbol]
          , TextSpan [Attr "x" <| AttrFloat <| x1 + bracketWidth] [TextString endSymbolSep]
          ]
        )
      ps -> do
        ((_, (objectHeight, objectWidth)), obj) <- mapAccumLM
          (drawObject fun)
          (coords { y = y + bracketHeight, x1 = x2 + identation }, (0, 0))
          (zipWithSeparator ls)
        return
          ( (objectHeight + bracketHeight + bracketHeightSep, objectWidth)
          , TextSpan
            []
            [ TextSpan [Attr "x" <| AttrFloat x1] [TextString startSymbol]
            , TextSpan []                         obj
            , TextSpan [Attr "x" <| AttrFloat x2, Attr "y" <| AttrFloat (y + bracketHeight + objectHeight)]
                       [TextString endSymbolSep]
            ]
          )

  drawObject
    :: Font a
    => (ObjectScope -> String -> b -> IntermediateT a ((Height, Width), TextContent))
    -> (ObjectScope, (Height, Width))
    -> (b, String)
    -> IntermediateT a ((ObjectScope, (Height, Width)), TextContent)
  drawObject tranformer (coords@ObjectScope {..}, (objectHeight, objectWidth)) (pair, separator) = do
    ((objHeight, objWidth), obj) <- tranformer coords separator pair
    return ((coords { y = y + objHeight }, (objHeight + objectHeight, max objWidth objectWidth)), obj)

  zipWithSeparator :: [a] -> [(a, String)]
  zipWithSeparator []       = []
  zipWithSeparator [e     ] = [(e, "")]
  zipWithSeparator (e : es) = (e, ",") : zipWithSeparator es

drawJsonPair :: Font a => ObjectScope -> String -> (String, JsonValue) -> IntermediateT a ((Height, Width), TextContent)
drawJsonPair coords@ObjectScope {..} pairSeparator (key, jsonValue) = do
  currentFont <- gets font
  let separatorCharSize = getStringSize currentFont separator
      txt               = wrapInQuote key
      keySize           = getStringSize currentFont txt
      leftSpace         = x2 + identation
      separatorX        = leftSpace + infoWidth keySize
      rightX            = separatorX + infoWidth separatorCharSize
  ((valueHeight, valueWidth), value) <- drawJsonValue (coords { x1 = rightX }) pairSeparator jsonValue
  return
    ( ( max valueHeight <| max (infoHeight separatorCharSize) (infoHeight keySize)
      , calculateWidth jsonValue valueWidth <| infoWidth separatorCharSize + separatorX
      )
    , TextSpan [Attr "x" (AttrFloat leftSpace), Attr "y" <| AttrFloat y]
               [TextString txt, singleSpan separatorX y separator, TextSpan [Attr "x" (AttrFloat rightX)] [value]]
    )
 where

  calculateWidth (JsonObject _) = max
  calculateWidth _              = (+)

  separator = ":"

drawJsonValue :: Font a => ObjectScope -> String -> JsonValue -> IntermediateT a ((Height, Width), TextContent)
drawJsonValue ObjectScope {..} separator value = do
  currentFont <- gets font
  let computeValueFn = computeValue currentFont separator x1 y
  case value of
    JsonString str -> computeValueFn (wrapInQuote str)
    JsonObject obj -> do
      (size, objElem) <- drawJsonObj x1 (identation + x2) y obj separator
      return (size, TextSpan [Attr "y" (AttrFloat y)] [objElem])
    JsonInteger int                        -> computeValueFn <| show int
    JsonBoolean bool                       -> computeValueFn <| boolToString bool
    JsonNullTk                             -> computeValueFn "null"
    JsonDouble     float                   -> computeValueFn <| show float
    JsonIdentifier (Identifier identifier) -> computeValueFn identifier
 where
  computeValue font separator x y str = do
    let sepStr  = str <> separator
        strSize = getStringSize font sepStr
    return ((infoHeight strSize, infoWidth strSize), singleSpan x y sepStr)

singleSpan x y txt = TextSpan [Attr "x" (AttrFloat x), Attr "y" (AttrFloat y)] [TextString txt]

wrapInQuote :: String -> String
wrapInQuote str = "\"" <> str <> "\""
