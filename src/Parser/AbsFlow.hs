module AbsFlow where

import           AbsData
import           Data.Map
import           GHC.Generics

data Info = Info {
  row :: Int,
  column :: Int,
  src :: String
} deriving (Show, Eq)

data Error = Error {
  row :: Int,
  column :: Int,
  message :: String,
  src :: String
}

instance Show Error where
  show Error{..} = Prelude.foldl (<>) "" errors
    where
      errors = [ "{\"type\":\"error\", ",
                "\"row\":",
                show row,
                ", \"column\":",
                show column,
                ", \"message\":",
                show message,
                ", \"src\": \"",
                src,
                "\"}" ]

instance Semigroup Text where
  (<>) a b = Text {t = t a <> t b}

instance Monoid Text where
  mempty = Text {t = []}

type EntityName = String

newtype Text = Text
  { t :: [String]
  } deriving (Eq, Ord, Show)

data EntityDef = EntityDef
  { name :: String
  , text :: Text
  , prevs :: [EntityName]
  } deriving (Eq, Ord, Show)

data StartNote
  = NoteAbove
  | NoteLeft
  | NoteRigth
  deriving (Eq, Ord, Show)

data Head =
  Head String
       [Assignment]
  deriving (Eq, Ord, Show)

type ResponseEntity = RequestEntity

data RequestEntity =
  RequestEntity String
                (Maybe String)
  deriving (Eq, Ord, Show)

data EntityTable = EntityTable
  { entitiesMap :: Map String EntityDef
  , allEntities :: [EntityName]
  } deriving (Eq, Ord, Show)


data Diagram a =
  DiagramRule a [Flow a]
  deriving (Eq, Ord, Show)

data Flow a
  = Group a Text
          [Flow a]
  | Comment a String
  | Requests a (Request a)
  | SetTo a String
          Value
  | Header a Head
  | Title a Text
  | Participant a String (Maybe String)
  | Delay a Text
  | Include a String
           (Map String String)
           [Flow a]
  | Note a StartNote
         [String]
         Text
  | Destroy a String
  deriving (Eq, Ord, Show)

data GroupType = Loop | Optional | Alternative
  deriving (Eq, Ord, Show)

data Request a =
  GroupRequest a GroupType [LabeledGroupRequest a]
  | Request a (BasicRequest a)
          [Flow a]
          (Maybe (BasicRequest a))
  deriving (Eq, Ord, Show)

data BasicRequest a =
  BasicRequest a RequestEntity
                 Arrows
                 ResponseEntity
                 Text
                 (Maybe RequestData)
  deriving (Eq, Ord, Show)

data LabeledGroupRequest a = LabeledGroupRequest a Text [Flow a]
  deriving (Eq, Ord, Show)

data DrawType
  = Thick
  | Dash
  deriving (Eq, Ord, Show)

data Arrows
  = Arrow DrawType --                 ->  or -->
  | NewParticipantArrow DrawType --   ->* or -->*
  | NewBlockArrow DrawType --         ->+  or -->+
  | ClosingBlockArrow DrawType --     ->-  or -->-
  | FailedArrow DrawType
  deriving (Eq, Ord, Show)
