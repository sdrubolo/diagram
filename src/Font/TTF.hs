{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module TTF
  ( TTF(..)
  , UShort
  )
where

import           Control.Monad
import qualified Data.ByteString.Lazy          as B
import           Data.Bits
import           Data.Word
import           Data.Binary.Get
import           Data.Map                      as Map
import           Data.ByteString.Char8          ( unpack )
import qualified Data.ByteString.Lazy.Char8    as LC
import           Data.Int
import qualified Data.Vector                   as V
import qualified Data.Text.Lazy                as T
import           Data.Text.Lazy.Encoding
import           Data.Text.Encoding.Error
import           Flow
import           Data.List
import           Debug.Trace
import           Data.Maybe
import           Control.Applicative
import           Data.Bifunctor
import           Font
import           Data.Tuple.Extra




type UShort = Word16
type Fixed = Word32
type ULong = Word32
type Byte = Word8
type Short = Int16
type FWord = Int16
type UFWord = Word16
type Char = Int8

getFixed :: Get Fixed
getFixed = fmap fromIntegral getWord32be


getUShort :: Get UShort
getUShort = getWord16be

getShort :: Get Short
getShort = fmap fromIntegral getWord16be

getULong :: Get ULong
getULong = getWord32be

getByte :: Get Byte
getByte = getWord8

getUFWord :: Get UFWord
getUFWord = getUShort

getFWord :: Get FWord
getFWord = getShort

getChar :: Get TTF.Char
getChar = fmap fromIntegral getWord8

-- format 2.14
getUFixed :: Get Double
getUFixed = fmap ((/ 0x4000) . fromIntegral) getShort

substr :: Int64 -> Int64 -> B.ByteString -> B.ByteString
substr s l = B.take l . B.drop s

diff :: Num b => [b] -> [b]
diff l = zipWith (-) l $ tail l


--- common parsing utils
data TableDirectory = TableDirectory { tDTag      :: String
                                     , tDCheckSum :: ULong
                                     , tDOffset   :: ULong
                                     , tDLength   :: ULong
                                     , tDRawData  :: B.ByteString
                                     } deriving (Show)

data TTF = TTF { version          :: Fixed
               , numTables        :: UShort
               , searchRange      :: UShort
               , entrySelector    :: UShort
               , rangeShift       :: UShort
               , tableDirectories :: Map.Map String TableDirectory
               , os2              :: OS2
               , head             :: Head
               , hhea             :: Hhea

               , name             :: Name
               , cmap             :: Cmap
               , maxp             :: Maxp
               , loca             :: Loca
               , hmtx             :: Hmtx
               , glyfs            :: V.Vector Glyf
               , kern             :: Maybe Kern
               } deriving (Show)


data Name = Name { formatSelector        :: UShort
                  , numberOfNameRecords :: UShort
                  , storageOffset       :: UShort
                  , nameRecords         :: [NameRecord]
                } deriving (Show)

data Maxp = Maxp { maxVersion            :: Fixed
                 , numGlyphs             :: UShort
                 , maxPoints             :: UShort
                 , maxContours           :: UShort
                 , maxCompositePoints    :: UShort
                 , maxCompositeContours  :: UShort
                 , maxZones              :: UShort
                 , maxTwilightPoints     :: UShort
                 , maxStorage            :: UShort
                 , maxFunctionDefs       :: UShort
                 , maxInstructionDefs    :: UShort
                 , maxStackElements      :: UShort
                 , maxSizeOfInstructions :: UShort
                 , maxComponentElements  :: UShort
                 , maxComponentDepth     :: UShort
                 } deriving (Show)


data CompositeGlyfElement = CompositeGlyfElement {cFlags       :: UShort
                                                , cGlyphIndex :: UShort
                                                , cXoffset    :: Short
                                                , cYoffset    :: Short
                                                , cArgument1  :: Short
                                                , cArgument2  :: Short
                                                , cXScale     :: Double
                                                , cYScale     :: Double
                                                , cScale01    :: Double
                                                , cScale10    :: Double
                                                } deriving (Show)

data Glyf =  EmptyGlyf |
             SimpleGlyf { sNumberOfContours    :: Short
                        , sXMin              :: FWord
                        , sYMin              :: FWord
                        , sXMax              :: FWord
                        , sYMax              :: FWord
                        , sEndPtsOfCountours :: [UShort]
                        , sInstructionLength :: UShort
                        , sInstructions      :: [Byte]
                        , sFlags             :: [Byte]
                        , sXCoordinates      :: [Short]
                        , sYCoordinates      :: [Short]
                      } |
             CompositeGlyf { cNumberOfContours :: Short
                           , cXMin             :: FWord
                           , cYMin             :: FWord
                           , cXMax             :: FWord
                           , cYMax             :: FWord
                           , cGlyfs            :: [CompositeGlyfElement]
                           , cNumInstruction   :: UShort
                           , cInstructions     :: [Byte]
                           } deriving (Show)

data KernPair = KernPair { kpLeft       :: UShort
                          , kpRight    :: UShort
                          , kpValue    :: Short
                          , kTCoverage :: UShort } deriving (Show)

data KernTable = KernSubTable0 { kNPairs        :: UShort
                              , kSearchRange   :: UShort
                              , kEntrySelector :: UShort
                              , kRangeShift    :: UShort
                              , kKernPairs     :: [KernPair] }
              | KernUnknown deriving (Show)

data Kern =  Kern { kernVersion           :: UShort
                 , kernNumberOfSubtables :: UShort
                 , kernTables            :: [KernTable]
                 } deriving (Show)

newtype Loca = Loca { locaOffsets :: [ULong] } deriving (Show)

data Hmtx = Hmtx { hMetrics         :: V.Vector HMetric
                , leftSideBearings :: [FWord]
                } deriving (Show)

data HMetric = HMetric { advanceWidth :: UFWord
                         , lsb        :: FWord
                         } deriving (Show)

data Cmap = Cmap { cmapVersion         :: UShort
                 , numberOfSubtables   :: UShort
                 , encodingDirectories :: [CmapEncodingDirectory]
                 , subTables           :: [CmapTable]
                 } deriving (Show)

data CmapTable = CmapFormat0 { c0Format   :: UShort
                            , c0Length   :: UShort
                            , c0Version  :: UShort
                            , c0GlyphIDs :: [Byte]
                            } |
                CmapFormat4 { c4Format         :: UShort
                            , c4Length         :: UShort
                            , c4Language       :: UShort
                            , c4SegCountX2     :: UShort
                            , c4SearchRange    :: UShort
                            , c4EntrySelector  :: UShort
                            , c4RangeShift     :: UShort
                            , c4EndCodes       :: [UShort]
                            , c4ReservedPad    :: UShort
                            , c4StartCodes     :: [UShort]
                            , c4IdDeltas       :: [UShort]
                            , c4IdRangeOffsets :: [UShort]
                            , c4GlyphIds       :: [UShort]
                            } |
                CmapFormat6 { c6Format     :: UShort
                            , c6Length     :: UShort
                            , c6Version    :: UShort
                            , c6FirstCode  :: UShort
                            , c6EntryCount :: UShort
                            , c6GlyphIds   :: [UShort]
                            } |
                CmapFormat12 { c12Format   :: Fixed
                             , c12Length   :: ULong
                             , c12Language :: ULong
                             , c12NGroups  :: ULong
                             , c12Groups   :: [F12Group]
                             } deriving (Show)

data CmapEncodingDirectory = CmapEncodingDirectory { cmapPlatformId :: UShort
                                                   , cmapEncodingId :: UShort
                                                   , cmapOffset     :: ULong
                                                  } deriving (Show)

data OS2 = OS2 { os2Version          :: UShort
              , xAvgCharWidth       :: Short
              , usWeightClass       :: UShort
              , usWidthClass        :: UShort
              , fsType              :: UShort
              , ySubscriptXSize     :: Short
              , ySubscriptYSize     :: Short
              , ySubscriptXOffset   :: Short
              , ySubscriptYOffset   :: Short
              , ySuperscriptXSize   :: Short
              , ySuperscriptYSize   :: Short
              , ySuperscriptXOffset :: Short
              , ySuperscriptYOffset :: Short
              , yStrikeoutSize      :: Short
              , yStrikeoutPosition  :: Short
              , sFamilyClass        :: Short
              , panose              :: [Byte]
              , ulUnicodeRange1     :: ULong
              , ulUnicodeRange2     :: ULong
              , ulUnicodeRange3     :: ULong
              , ulUnicodeRange4     :: ULong
              , aschVendID          :: [Byte]
              , fsSelection         :: UShort
              , usFirstCharIndex    :: UShort
              , usLastCharIndex     :: UShort
              , sTypoAscender       :: UShort
              , sTypoDescender      :: UShort
              , sTypoLineGap        :: UShort
              , usWinAscent         :: UShort
              , usWinDescent        :: UShort
              , ulCodePageRange1    :: ULong
              , ulCodePageRange2    :: ULong
              } deriving (Show)

data Head = Head { headVersion       :: Fixed
                 , fontRevision      :: Fixed
                 , checkSumAdjusment :: ULong
                 , magicNumber       :: ULong
                 , headFlags         :: UShort
                 , unitsPerEm        :: UShort
                 , created           :: B.ByteString
                 , modified          :: B.ByteString
                 , xMin              :: FWord
                 , yMin              :: FWord
                 , xMax              :: FWord
                 , yMax              :: FWord
                 , macStyle          :: UShort
                 , lowestRecPPEM     :: UShort
                 , fontDirectionHint :: Short
                 , indexToLocFormat  :: Short
                 , glyphDataFormat   :: Short
                 } deriving (Show)

data Hhea = Hhea { hheaVersion         :: Fixed
                 , ascender            :: FWord
                 , descender           :: FWord
                 , lineGap             :: FWord
                 , advanceWidthMax     :: UFWord
                 , minLeftSideBearing  :: FWord
                 , minRightSideBearing :: FWord
                 , xMaxExtend          :: FWord
                 , caretSlopeRise      :: Short
                 , caretSlopeRun       :: Short
                  -- reserved 5 Short
                 , metricDataFormat    :: Short
                 , numberOfHMetrics    :: UShort
                } deriving (Show)

data F12Group = F12Group { f12StartCharCode :: ULong
                         , f12EndCharCode   :: ULong
                         , f12StartGlyphId  :: ULong
                         } deriving (Show)

data NameRecord = NameRecord { platformId   :: UShort
                             , encodingId :: UShort
                             , languageId :: UShort
                             , nameId     :: UShort
                             , strLength  :: UShort
                             , strOffset  :: UShort
                             , str        :: T.Text
                             } deriving (Show)

parseTable :: String -> Get a -> Map.Map String TableDirectory -> B.ByteString -> a
parseTable name' m tds font = fromJust <| parseMaybeTable name' m tds font

parseMaybeTable :: String -> Get a -> Map.Map String TableDirectory -> B.ByteString -> Maybe a
parseMaybeTable name' m tds font =
  let f t = runGet
        (do
          skip <| fromIntegral <| tDOffset t
          m
        )
        font
  in  fmap f (Map.lookup name' tds)


parseTableDirectory :: B.ByteString -> Get TableDirectory
parseTableDirectory font = do
  tDTag      <- unpack <$> getByteString 4
  tDCheckSum <- getULong
  tDOffset   <- getULong
  tDLength   <- getULong
  let tDRawData = substr (fromIntegral tDOffset) (fromIntegral tDLength) font
  return TableDirectory {..}

parseTableDirectories :: B.ByteString -> Int -> Get (Map.Map String TableDirectory)
parseTableDirectories font n = do
  list <- replicateM n $ parseTableDirectory font
  return <| fromList <| Prelude.map (\x -> (tDTag x, x)) list

kernPairs :: KernTable -> [KernPair]
kernPairs KernSubTable0 { kKernPairs = pairs } = pairs
kernPairs KernUnknown                          = []

parseKern :: Map String TableDirectory -> B.ByteString -> Maybe Kern
parseKern = parseMaybeTable
  "kern"
  (do
    kernVersion           <- getUShort
    kernNumberOfSubtables <- getUShort
    kernTables            <- replicateM (fromIntegral kernNumberOfSubtables) parseKernTable
    return Kern {..}
  )

parseKernSubTable :: UShort -> Int -> Int -> Get KernTable
parseKernSubTable kTCoverage _ 0 = do
  kNPairs        <- getUShort
  kSearchRange   <- getUShort
  kEntrySelector <- getUShort
  kRangeShift    <- getUShort
  kKernPairs     <- replicateM (fromIntegral kNPairs) parseKernPair
  return KernSubTable0 {..}
 where
  parseKernPair = do
    kpLeft  <- getUShort
    kpRight <- getUShort
    kpValue <- getShort
    return KernPair {..}

parseKernSubTable _ length' _version = do
  skip (length' - 6)
  return KernUnknown

parseKernTable :: Get KernTable
parseKernTable = do
  kTLength   <- getULong
  kTCoverage <- getUShort
  parseKernSubTable kTCoverage (fromIntegral kTLength) $ shiftR (fromIntegral kTCoverage) 8

parseFlags :: Int -> [Byte] -> Get [Byte]
parseFlags n a
  | n <= 0 = return a
  | otherwise = do
    flag <- getByte
    if testBit flag 3
      then do
        repeats <- fmap fromIntegral getByte
        parseFlags (n - repeats - 1) $ a ++ replicate repeats flag ++ [flag]
      else parseFlags (n - 1) $ a ++ [flag]

parseCoordinate :: Int -> Int -> (Short, [Short]) -> Byte -> Get (Short, [Short])
parseCoordinate shortBit sameBit (current, ac) flag
  | testBit flag shortBit = do
    delta <- fmap fromIntegral getByte
    return
      (if testBit flag sameBit then (current + delta, current + delta : ac) else (current - delta, current - delta : ac))
  | otherwise = if testBit flag sameBit
    then return (current, current : ac)
    else do
      delta <- getShort
      return (current + delta, current + delta : ac)


parseCoordinates :: [Byte] -> Int -> Int -> Get [Short]
parseCoordinates flags shortBit sameBit = reverse . snd <$> foldM (parseCoordinate shortBit sameBit) (0, []) flags

parseCompositeGlyfElement :: Get CompositeGlyfElement
parseCompositeGlyfElement = do
  cFlags      <- getUShort
  cGlyphIndex <- getUShort
  cArgument1  <- getArg cFlags
  cArgument2  <- getArg cFlags
  let cScale01 = 0.0
      cScale10 = 0.0
      cXScale  = 1.0
      cYScale  = 1.0
      cXoffset | testBit cFlags args_are_xy_values = cArgument1
               | otherwise                         = 0
      cYoffset | testBit cFlags args_are_xy_values = cArgument2
               | otherwise                         = 0
  if testBit cFlags we_have_a_scale
    then do
      cXScale <- fmap fromIntegral getUShort
      let cYScale = cXScale
      return CompositeGlyfElement {..}
    else if testBit cFlags we_have_an_x_and_y_scale
      then do
        cXScale <- getUFixed
        cYScale <- getUFixed
        return CompositeGlyfElement {..}
      else if testBit cFlags we_have_a_two_by_tow
        then do
          cXScale  <- getUFixed
          cScale01 <- getUFixed
          cScale10 <- getUFixed
          cYScale  <- getUFixed
          return CompositeGlyfElement {..}
        else return CompositeGlyfElement {..}
 where
  getArg f | testBit f arg_1_and_2_are_words = fmap fromIntegral getUShort
           | otherwise                       = fmap fromIntegral TTF.getChar
  arg_1_and_2_are_words    = 0
  args_are_xy_values       = 1
  we_have_a_scale          = 3
  we_have_an_x_and_y_scale = 6
  we_have_a_two_by_tow     = 7

parseGlyf :: Short -> Get Glyf
parseGlyf numberOfContours
  | numberOfContours >= 0 = do
    sXMin              <- getFWord
    sYMin              <- getFWord
    sXMax              <- getFWord
    sYMax              <- getFWord
    sEndPtsOfCountours <- replicateM (fromIntegral numberOfContours) getUShort
    sInstructionLength <- getUShort
    sInstructions      <- replicateM (fromIntegral sInstructionLength) getByte
    let count = if numberOfContours == 0 then 0 else fromIntegral $ last sEndPtsOfCountours + 1
    sFlags        <- parseFlags count []
    sXCoordinates <- parseCoordinates sFlags 1 4
    sYCoordinates <- parseCoordinates sFlags 2 5
    return SimpleGlyf {sNumberOfContours = numberOfContours, ..}
  | otherwise = do
    cXMin  <- getFWord
    cYMin  <- getFWord
    cXMax  <- getFWord
    cYMax  <- getFWord
    cGlyfs <- parseElements
    let lastFlag        = cFlags $ last cGlyfs
        cNumInstruction = 0
        cInstructions   = []
    if testBit lastFlag we_have_instructions
      then do
        cNumInstruction <- getUShort
        cInstructions   <- replicateM (fromIntegral cNumInstruction) getByte
        return CompositeGlyf {cNumberOfContours = numberOfContours, ..}
      else return CompositeGlyf {cNumberOfContours = numberOfContours, ..}
 where
  parseElements = do
    cge <- parseCompositeGlyfElement
    if testBit (cFlags cge) more_components
      then do
        rest <- parseElements
        return $ cge : rest
      else return [cge]
  more_components      = 5
  we_have_instructions = 8



parseGlyfs :: Int -> [Int] -> Map String TableDirectory -> B.ByteString -> [Glyf]
parseGlyfs glyphCount offsets tds font = zipWith getGlyph (Data.List.take glyphCount offsets) $ diff offsets
 where
  tableStart = fromIntegral . tDOffset $ tds ! "glyf"
  getGlyph _      0    = EmptyGlyf
  getGlyph offset _len = runGet
    (do
      skip $ tableStart + offset
      numberOfContours <- getShort
      parseGlyf numberOfContours
    )
    font

parseHead :: Map.Map String TableDirectory -> B.ByteString -> Head
parseHead = parseTable
  "head"
  (do
    headVersion       <- getFixed
    fontRevision      <- getFixed
    checkSumAdjusment <- getULong
    magicNumber       <- getULong
    headFlags         <- getUShort
    unitsPerEm        <- getUShort
    created           <- getLazyByteString 8
    modified          <- getLazyByteString 8
    xMin              <- getFWord
    yMin              <- getFWord
    xMax              <- getFWord
    yMax              <- getFWord
    macStyle          <- getUShort
    lowestRecPPEM     <- getUShort
    fontDirectionHint <- getShort
    indexToLocFormat  <- getShort
    glyphDataFormat   <- getShort
    return Head {..}
  )

parseHhea :: Map.Map String TableDirectory -> B.ByteString -> Hhea
parseHhea = parseTable
  "hhea"
  (do
    hheaVersion         <- getFixed
    ascender            <- getFWord
    descender           <- getFWord
    lineGap             <- getFWord
    advanceWidthMax     <- getUFWord
    minLeftSideBearing  <- getFWord
    minRightSideBearing <- getFWord
    xMaxExtend          <- getFWord
    caretSlopeRise      <- getShort
    caretSlopeRun       <- getShort
    replicateM_ 5 getShort
    metricDataFormat <- getShort
    numberOfHMetrics <- getUShort
    return Hhea {..}
  )

parseOS2 :: Map.Map String TableDirectory -> B.ByteString -> OS2
parseOS2 = parseTable
  "OS/2"
  (do
    os2Version <- getUShort
    unless (os2Version `elem` [1 .. 4]) (error $ "unhandled  os2 version " ++ show os2Version)
    xAvgCharWidth       <- getShort
    usWeightClass       <- getUShort
    usWidthClass        <- getUShort
    fsType              <- getUShort
    ySubscriptXSize     <- getShort
    ySubscriptYSize     <- getShort
    ySubscriptXOffset   <- getShort
    ySubscriptYOffset   <- getShort
    ySuperscriptXSize   <- getShort
    ySuperscriptYSize   <- getShort
    ySuperscriptXOffset <- getShort
    ySuperscriptYOffset <- getShort
    yStrikeoutSize      <- getShort
    yStrikeoutPosition  <- getShort
    sFamilyClass        <- getShort
    panose              <- replicateM 10 getByte
    ulUnicodeRange1     <- getULong
    ulUnicodeRange2     <- getULong
    ulUnicodeRange3     <- getULong
    ulUnicodeRange4     <- getULong
    aschVendID          <- replicateM 4 getByte
    fsSelection         <- getUShort
    usFirstCharIndex    <- getUShort
    usLastCharIndex     <- getUShort
    sTypoAscender       <- getUShort
    sTypoDescender      <- getUShort
    sTypoLineGap        <- getUShort
    usWinAscent         <- getUShort
    usWinDescent        <- getUShort
    ulCodePageRange1    <- getULong
    ulCodePageRange2    <- getULong
    return OS2 {..}
  )

parseName :: Map String TableDirectory -> B.ByteString -> Name
parseName tds font = runGet
  (do
    let tableStart = fromIntegral <| tDOffset <| tds ! "name"
    skip tableStart
    formatSelector      <- getUShort
    numberOfNameRecords <- getUShort
    storageOffset       <- getUShort
    nameRecords         <- replicateM (fromIntegral numberOfNameRecords)
      $ parseNameRecord font (tableStart + fromIntegral storageOffset)
    return Name {..}
  )
  font


parseCmapEncodingDirectory :: Get CmapEncodingDirectory
parseCmapEncodingDirectory = do
  cmapPlatformId <- getUShort
  cmapEncodingId <- getUShort
  cmapOffset     <- getULong
  return CmapEncodingDirectory {..}

parseCmapEncoding :: B.ByteString -> Int -> CmapTable
parseCmapEncoding font offset = runGet
  (do
    skip offset
    format <- getUShort
    parseCmapSubTable $ fromIntegral format
  )
  font

parseCmapSubTable :: Int -> Get CmapTable
parseCmapSubTable 4 = do
  c4Length     <- getUShort
  c4Language   <- getUShort
  c4SegCountX2 <- getUShort
  let segCount = fromIntegral c4SegCountX2 `div` 2
  c4SearchRange    <- getUShort
  c4EntrySelector  <- getUShort
  c4RangeShift     <- getUShort
  c4EndCodes       <- replicateM segCount getUShort
  c4ReservedPad    <- getUShort
  c4StartCodes     <- replicateM segCount getUShort
  c4IdDeltas       <- replicateM segCount getUShort
  c4IdRangeOffsets <- replicateM segCount getUShort
  let glyphCount = (fromIntegral c4Length - (2 * 8) - (2 * segCount * 4)) `div` 2
  c4GlyphIds <- replicateM glyphCount getUShort
  return CmapFormat4 {c4Format = 4, ..}

parseCmapSubTable 0 = do
  c0Length   <- getUShort
  c0Version  <- getUShort
  c0GlyphIDs <- replicateM 256 getByte
  return CmapFormat0 {c0Format = 0, ..}

parseCmapSubTable 6 = do
  c6Length     <- getUShort
  c6Version    <- getUShort
  c6FirstCode  <- getUShort
  c6EntryCount <- getUShort
  c6GlyphIds   <- replicateM (fromIntegral c6EntryCount) getUShort
  return CmapFormat6 {c6Format = 6, ..}

parseCmapSubTable 12 = do
  _           <- getUShort
  c12Length   <- getULong
  c12Language <- getULong
  c12NGroups  <- getULong
  c12Groups   <- replicateM (fromIntegral c12NGroups) parseF12Group
  return CmapFormat12 {c12Format = 12, ..}
 where
  parseF12Group = do
    f12StartCharCode <- getULong
    f12EndCharCode   <- getULong
    f12StartGlyphId  <- getULong
    return F12Group {..}

parseCmapSubTable n = error $ "subtable format not implemented " ++ show n

parseHMetric :: Get HMetric
parseHMetric = do
  advanceWidth <- getUFWord
  lsb          <- getFWord
  return HMetric {..}


parseHmtx :: Int -> Int -> Map String TableDirectory -> B.ByteString -> Hmtx
parseHmtx mcount glyphCount = parseTable
  "hmtx"
  (do
    hMetrics         <- V.fromList <$> replicateM mcount parseHMetric
    leftSideBearings <- replicateM (glyphCount - mcount) getShort
    return Hmtx {..}
  )


parseNameRecord :: B.ByteString -> Int -> Get NameRecord
parseNameRecord font storageOffset = do
  platformId <- getUShort
  encodingId <- getUShort
  languageId <- getUShort
  nameId     <- getUShort
  strLength  <- getUShort
  strOffset  <- getUShort
  let
    str =
      decoder platformId encodingId
        <| substr (fromIntegral ((fromIntegral storageOffset :: Int) + fromIntegral strOffset)) (fromIntegral strLength) font
  return NameRecord {..}
 where
  decoder 3 _ = decodeUtf16BE
  decoder 2 _ = decodeUtf16BE
  decoder 1 _ = decodeUtf8With ignore
  decoder 0 _ = decodeUtf16BE
  decoder _ _ = decodeUtf16BE

parseCmap :: Map String TableDirectory -> B.ByteString -> Cmap
parseCmap tds font = runGet
  (do
    let tableStart = fromIntegral <| tDOffset $ tds ! "cmap"
    skip tableStart
    cmapVersion         <- getUShort
    numberOfSubtables   <- getUShort
    encodingDirectories <- replicateM (fromIntegral numberOfSubtables) parseCmapEncodingDirectory
    let subTables = Prelude.map (parseCmapEncoding font . (+ tableStart) . fromIntegral . cmapOffset) encodingDirectories
    return Cmap {..}
  )
  font


parseLoca :: Int -> Int -> Map String TableDirectory -> B.ByteString -> Loca
parseLoca 0 count = parseTable
  "loca"
  (do
    locaOffsets <- replicateM (count + 1) (fmap ((*) 2 . fromIntegral) getUShort)
    return Loca {..}
  )
parseLoca 1 count = parseTable
  "loca"
  (do
    locaOffsets <- replicateM (count + 1) getULong
    return Loca {..}
  )
parseLoca _ _ = error "error while parsing loca table"


parseMaxp :: Map String TableDirectory -> B.ByteString -> Maxp
parseMaxp = parseTable
  "maxp"
  (do
    maxVersion            <- getFixed
    numGlyphs             <- getUShort
    maxPoints             <- getUShort
    maxContours           <- getUShort
    maxCompositePoints    <- getUShort
    maxCompositeContours  <- getUShort
    maxZones              <- getUShort
    maxTwilightPoints     <- getUShort
    maxStorage            <- getUShort
    maxFunctionDefs       <- getUShort
    maxInstructionDefs    <- getUShort
    maxStackElements      <- getUShort
    maxSizeOfInstructions <- getUShort
    maxComponentElements  <- getUShort
    maxComponentDepth     <- getUShort
    return Maxp {..}
  )

ttfParse :: B.ByteString -> TTF
ttfParse font = runGet
  (do
    version          <- getFixed
    numTables        <- getUShort
    searchRange      <- getUShort
    entrySelector    <- getUShort
    rangeShift       <- getUShort
    tableDirectories <- parseTableDirectories font (fromIntegral numTables)
    let os2        = parseOS2 tableDirectories font
        head       = parseHead tableDirectories font
        hhea       = parseHhea tableDirectories font
        name       = parseName tableDirectories font
        cmap       = parseCmap tableDirectories font
        maxp       = parseMaxp tableDirectories font
        kern       = parseKern tableDirectories font
        glyphCount = fromIntegral <| numGlyphs maxp
        loca       = parseLoca (fromIntegral <| indexToLocFormat head) glyphCount tableDirectories font
        hmtx       = parseHmtx (fromIntegral <| numberOfHMetrics hhea) glyphCount tableDirectories font
        glyfs      = V.fromList <| parseGlyfs glyphCount (Prelude.map fromIntegral <| locaOffsets loca) tableDirectories font
    return TTF {..}
  )
  font

ttfFontName :: TTF -> String
ttfFontName font = findFontFamily <| nameRecords <| TTF.name font
 where
  findFontFamily l =
    maybe "NOT_FOUND" (T.unpack . str)
      <| find (\NameRecord {..} -> (platformId == 0 || platformId == 1 || platformId == 3) && nameId == 1) l

instance Font TTF where
  charSize = ttfCharSize
  parse = ttfParse
  lineGap = ttfLineGap
  fontName = ttfFontName

ttfUnits = fromIntegral . unitsPerEm . TTF.head

ttfLineGap size ttf = unitsPerEmToPixel size (ttfUnits ttf) (TTF.lineGap <| hhea ttf)

getInt :: (Integral a) => a -> Int
getInt = fromIntegral

getGlyfMetrics :: Int -> TTF -> HMetric
getGlyfMetrics index ttf = if metricsNo <= index then V.last metricsVector else metricsVector V.! index
 where
  metricsVector = hMetrics <| hmtx ttf
  metricsNo     = fromIntegral <| numberOfHMetrics <| hhea ttf


getGlyfIndex :: TTF -> Get Int
getGlyfIndex ttf = do
  charCode <- fmap fromIntegral getUShort
  let index =
        Prelude.foldr (\table acc -> acc <|> (fmap getInt <| findGlyfIndex charCode table)) Nothing (subTables <| cmap ttf)
  return <| fromMaybe 0 index
 where
  findGlyfIndex charCode CmapFormat0 {..} | charCode >= 32 && charCode <= 126 =
    let i = c0GlyphIDs !! fromIntegral charCode in Just <| fromIntegral i
  findGlyfIndex _        CmapFormat0 {..}          = Nothing
  findGlyfIndex charCode subtable@CmapFormat4 {..} = if charCode >= c4StartCodes !! i
    then Just <| getGlyphIdenx charCode i rangeOffset subtable
    else Nothing
   where
    getGlyphIdenx charCode i 0 CmapFormat4 {..} = (c4IdDeltas !! i + charCode) `mod` 65535
    getGlyphIdenx charCode i n CmapFormat4 {..} = c4GlyphIds !! (getInt charCode - getInt (c4StartCodes !! i))
    rangeOffset = getInt <| c4IdRangeOffsets !! i
    i           = fromJust <| Data.List.findIndex (>= charCode) c4EndCodes
  findGlyfIndex charCode CmapFormat6 {..} | c6FirstCode <= charCode && charCode <= c6EntryCount =
    Just <| (c6GlyphIds !! getInt charCode)
  findGlyfIndex _ CmapFormat6 {..}  = Nothing
  findGlyfIndex _ CmapFormat12 {..} = Nothing


ttfCharSize :: Float -> TTF -> Prelude.Char -> Size
ttfCharSize size ttf char = runGet
  (do
    index <- getGlyfIndex ttf
    let metrics      = getGlyfMetrics index ttf
        units        = ttfUnits ttf
        _width       = unitsPerEmToPixel size units <| advanceWidth metrics
        emToPixel    = unitsPerEmToPixel size units
        charHeight   = getYCoords <| glyfs ttf V.! index
        (yMin, yMax) = both (unitsPerEmToPixel size units) charHeight
    return Size {..}
  )
  (encodeUtf16BE <| T.singleton char)
 where
  getYCoords EmptyGlyf          = (0, 0)
  getYCoords SimpleGlyf {..}    = (sYMin, sYMax)
  getYCoords CompositeGlyf {..} = (cYMin, cYMax)
