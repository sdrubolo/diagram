module PrintData where

import           AbsData
import           Data.Char
import           Print

instance Print (Maybe RequestData) where
  prt i Nothing = id
  prt i (Just e) = prt i e

instance Print RequestData where
  prt i e =
    case e of
      Json d -> prt 0 d
      Form d -> prt 0 d

instance Print Assignment where
  prt i e =
    case e of
      AssignmentByValue entity str -> prt i [e]
      AssignmentById entity str -> prt i [e]
  prtList _ [] = id
  prtList i (x:xs) =
    concatD
      [ doc (showString "`")
      , doc $ showString $ foldl compose (pr x) xs
      , doc (showString "`")
      ]
    where
      pr (AssignmentByValue entity str) =
        foldr ($) "" (prt i entity []) ++ "=" ++ foldr ($) "" (prt 0 str [])
      pr (AssignmentById entity str) =
        foldr ($) "" (prt i entity []) ++ "=${" ++ foldr ($) "" (prt 0 str []) ++ "}"
      compose acc x = acc ++ ";" ++ pr x

instance Print Value where
  prt i e =
    case e of
      StringValue str -> prt 0 str
      IntegerValue n -> prt 0 n
      BoolValue boolean -> prt 0 boolean

instance Print Identifier where
  prt i e =
    case e of
      Identifier entity -> doc (showString entity)

instance Print (String,JsonValue) where
  prt i (str,value) = concatD [prt 0 str, doc (showString ": "), prt 0 value]
  prtList _ [] = id
  prtList i (x:xs) = foldl compose (prt i x) xs
    where
      compose acc x = acc . concatD [doc $ showString ",", prt i x]


instance Print JsonValue where
  prt i e =
    case e of
      JsonObject object ->
        concatD [doc (showString "{"), prt 0 object, doc (showString "}")]
      JsonArray array ->
        concatD [doc (showString "["), prt 0 array, doc (showString "]")]
      JsonString str -> prt 0 str
      JsonInteger n -> prt 0 n
      JsonBoolean booleantk -> prt 0 booleantk
      JsonNullTk -> doc $ showString "null"
      JsonDouble d -> prt 0 d
      JsonIdentifier a -> prt 0 a
