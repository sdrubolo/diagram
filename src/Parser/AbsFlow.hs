module AbsFlow where

import           AbsData
import           Data.Map
import           GHC.Generics

data ParserInfo = ParserInfo {
  row :: Int,
  column :: Int,
  src :: String
} deriving (Show, Eq)


instance Semigroup Text where
  (<>) a b = Text {t = t a <> t b }

instance Monoid Text where
  mempty = Text {t = [] }

type EntityName = String

newtype Text = Text { t :: [String] } deriving (Eq, Ord, Show)

data EntityDef = EntityDef
  { name :: String
  , text :: Text
  , prevs :: [EntityName]
  } deriving (Eq, Ord, Show)

data Head =
  Head String
       [Assignment]
  deriving (Eq, Ord, Show)

type ResponseEntity = RequestEntity

data RequestEntity =
  RequestEntity String
                (Maybe String)
  deriving (Eq, Ord, Show)

data Diagram a =
  DiagramRule a [Flow a]
  deriving (Eq, Ord, Show)

data StartNote
  = NoteAbove
  | NoteLeft
  | NoteRight
  deriving (Eq, Ord, Show)

data Flow a
  = Group a Text
          [Flow a]
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
  | Note a StartNote [String] Text
  | Destroy a String
  | LabelledGroup a GroupType [Labelling a]
  deriving (Eq, Ord, Show)

data GroupType = Loop | Optional | Alternative
  deriving (Eq, Ord, Show)

data Request a = Request a (BasicRequest a)
                           [Flow a]
                           (Maybe (BasicRequest a))
  deriving (Eq, Ord, Show)

data Labelling a = Labelling a Text [Flow a]
  deriving (Eq, Ord, Show)

data BasicRequest a =
  BasicRequest a RequestEntity
                 Arrows
                 ResponseEntity
                 Text
                 (Maybe RequestData)
  deriving (Eq, Ord, Show)

data DrawType
  = Thick
  | Dash
  deriving (Eq, Ord, Show)

data Arrows
  = Arrow DrawType                    -- ->  or -->
  | NewParticipantArrow DrawType      -- ->* or -->*
  | NewParticipantBlockArrow DrawType -- ->*+ or -->*+
  | NewBlockArrow DrawType            -- ->+  or -->+
  | ClosingBlockArrow DrawType        -- ->-  or -->-
  | FailedArrow DrawType
  deriving (Eq, Ord, Show)
