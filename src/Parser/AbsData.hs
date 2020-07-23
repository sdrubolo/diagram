module AbsData where

import           Data.Char

data RequestData
  = Json JsonObj
  | Form [Assignment]
  deriving (Eq, Ord, Show)

data Value
  = BoolValue Bool
  | IntegerValue Integer
  | StringValue String
  deriving (Eq, Ord, Show)

newtype Identifier =
  Identifier String
  deriving (Eq, Ord, Show)

data Assignment
  = AssignmentByValue Identifier
                      Value
  | AssignmentById Identifier
                   Identifier
  deriving (Eq, Ord, Show)

data JsonObj
  = JsonObjObject [(String,JsonValue)]
  | JsonObjArray [JsonValue]
  deriving (Eq, Ord, Show)


data JsonValue
  = JsonString String
  | JsonObject JsonObj
  | JsonInteger Integer
  | JsonBoolean Bool
  | JsonNullTk
  | JsonDouble Float
  | JsonIdentifier Identifier
  deriving (Eq, Ord, Show)
