module Font
  ( Font(..)
  , FontTable(..)
  , SizeInfo(..)
  , Size(..)
  , getCharWidth
  , getCharSize
  , getStringWidth
  , getStringSize
  , getFont
  , unitsPerEmToPixel
  )
where

import           Data.ByteString.Lazy          as B
import           Flow
import qualified Data.Map                      as M
import           Data.Char
import           Debug.Trace
import           System.Directory


data Size = Size {
  _width :: Float,
  yMin :: Float,
  yMax :: Float
} deriving (Show, Eq)

data SizeInfo = SizeInfo {
  infoWidth :: Float,
  infoHeight :: Float
} deriving (Show, Eq)

data FontTable a = FontTable { name :: String
                             , lineHeigth :: Float
                             , src :: String
                             , fontSize :: Float
                             , fontLineGap :: Float
                             , table :: M.Map Char Size
                             , fontType :: String }

instance Semigroup SizeInfo where
  (<>) a b =
    SizeInfo {infoWidth = infoWidth a + infoWidth b, infoHeight = max (infoHeight a) (infoHeight b)}

instance Monoid SizeInfo where
  mempty = SizeInfo {infoWidth = 0, infoHeight = 0}

instance Semigroup Size where
  (<>) a b =
    Size {_width = _width a + _width b, yMax = max (yMax a) (yMax b), yMin = min (yMin a) (yMin b)}

instance Monoid Size where
  mempty = Size {_width = 0, yMin = 0, yMax = 0}


unitsPerEmToPixel :: (Integral a) => Float -> Float -> a -> Float
unitsPerEmToPixel size unit val = fromIntegral val * size / unit

class (Show a) =>
      Font a
  where
  charSize :: Float -> a -> Char -> Size
  lineGap :: Float -> a -> Float
  parse  :: ByteString -> a
  fontName :: a -> String

buildFontTable :: Font a => Float -> String -> a -> FontTable a
buildFontTable size src font = FontTable
  { name        = fontName font
  , src         = src
  , fontSize    = size
  , lineHeigth  = 2
  , fontLineGap = max 2 <| lineGap size font
  , table       = Prelude.foldl addCharSet M.empty charSet
  , fontType    = "truetype"
  }
 where
  addCharSet = Prelude.foldl (\acc i -> let c = chr i in M.insert c (charSize size font c) acc)
  charSet    = [[32 .. 126], [160 .. 591], [601, 636, 658], [688 .. 767], [880 .. 1023]]


getFont :: Font a => String -> Float -> IO (FontTable a)
getFont path size = do
  absolutePath <- canonicalizePath path
  font         <- parse <$> B.readFile absolutePath
  return <| buildFontTable size absolutePath font

instance Show (FontTable a) where
  show = name

getCharWidth :: Font a => FontTable a -> Char -> Float
getCharWidth font char = infoWidth <| getCharSize font char

getCharSize :: Font a => FontTable a -> Char -> SizeInfo
getCharSize font char = getStringSize font [char]

getStringWidth :: Font a => FontTable a -> String -> Float
getStringWidth font = sum . Prelude.map (getCharWidth font)

getStringSize :: Font a => FontTable a -> String -> SizeInfo
getStringSize font str =
  let Size {..} = Prelude.foldl (\acc char -> let v = M.findWithDefault mempty char (table font) in acc <> v) mempty str
  in  SizeInfo {infoWidth = _width, infoHeight = yMax - yMin + fontLineGap font + lineHeigth font}
