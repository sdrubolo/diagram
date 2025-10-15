module Txt
  ( TxtInfo(..)
  , Txt(..)
  , Width
  , computeText
  , computeString
  , linedText
  )
where

import           Font
import           AbsFlow
import           Data.Char                      ( isSpace )
import           Flow

type Width = Float

data TxtInfo = TxtInfo
  { txt_width  :: Width
  , txt_height :: Float
  , txt        :: String
  } deriving (Eq, Ord, Show)

data Txt = Txt
  { txt_info  :: [TxtInfo]
  , max_width :: Float
  } deriving (Eq, Ord, Show)

instance Semigroup Txt where
  (<>) a b =
    Txt
      { txt_info = txt_info a <> txt_info b
      , max_width = max (max_width a) (max_width b) }

instance Monoid Txt where
  mempty = Txt {txt_info=[], max_width=0}

computeString :: Font a => FontTable a -> String -> Txt
computeString font name =
  let SizeInfo {..} = getStringSize font name
      txt_info      = TxtInfo infoWidth infoHeight name
  in  Txt [txt_info] (txt_width txt_info)

computeText :: Font a => FontTable a -> Text -> Txt
computeText font Text { t = lines } = mconcat (map (computeString font) lines)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

linedText :: String -> Text
linedText txt = Text {t = map trim <| lines txt}
