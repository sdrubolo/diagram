module AbsParser
  ( parseString
  , parseFile
  )
where

import           AbsData
import           AbsFlow

import           AbsError
import qualified Control.Applicative.Combinators
                                               as C
import           Control.Monad                  ( guard
                                                , liftM2
                                                , void
                                                )
import           Data.Char                      ( isSpace )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.List                     as L
import           Data.Functor
import           Debug.Trace
import           Flow
import           Text.Parsec
import           Control.Monad.IO.Class
import           System.Directory
import           System.FilePath
import           Data.Maybe
import           Data.Tuple.HT
import           Text.Parsec.Char               ( endOfLine )
import           Data.Bifunctor
import           Text.ParserCombinators.Parsec.Error
import           Text.Read                      ( Lexeme(Char) )
import           Data.String                    ( IsString )



newtype ParserState = ParserState { fileName :: Maybe String } deriving (Show)

type FlowParser a = ParsecT String ParserState IO a

parseError :: ParseError -> Error
parseError error =
  let position = errorPos error
  in  AbsError.Error
        { src     = sourceName position
        , column  = sourceColumn position
        , row     = sourceLine position
        , message = foldr (\acc elm -> elm <> " " <> acc) ""
          .  lines
          <| showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input"
          <| errorMessages error
        }

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

linedText :: String -> Text
linedText txt = Text {t = map trim <| lines txt}

processEntity :: String -> FlowParser String
processEntity entity = return <| trim entity

whiteSpace :: FlowParser ()
whiteSpace = choice [void <| many1 (oneOf " \t\n") *> whiteSpace, return ()]

quotedString :: FlowParser String
quotedString = between (char '"') (char '"') (many quotedStringChar)
 where
  quotedStringChar = escapedChar <|> normalChar
  escapedChar      = char '\\' *> oneOf ['\\', '"']
  normalChar       = noneOf "\""

plus :: FlowParser [Char]
plus = char '+' *> number

number :: FlowParser [Char]
number = many1 digit

lettersTillDelimiter :: FlowParser a -> FlowParser [Char]
lettersTillDelimiter p = inLineWhiteSpace *> (removeEscapedNewLine . trim <$> parseTillDelimiter anyChar p) <* whiteSpace
 where
  removeEscapedNewLine :: [Char] -> [Char]
  removeEscapedNewLine []                = []
  removeEscapedNewLine ('\\' : 'n' : xs) = '\n' : removeEscapedNewLine xs
  removeEscapedNewLine (x          : xs) = x : removeEscapedNewLine xs

  inLineWhiteSpace :: FlowParser ()
  inLineWhiteSpace = choice [void <| many1 (oneOf " \t") *> inLineWhiteSpace, return ()]

parseTillDelimiter :: FlowParser a1 -> FlowParser a2 -> FlowParser [a1]
parseTillDelimiter s p = f
 where
  f =
    do
        try <| lookAhead <| void p
        return []
      <|> do
            c <- s
            v <- f
            return (c : v)

foldlP :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m d -> (a -> c -> c) -> c -> ParsecT s u m c
foldlP p op f x = foldPAux p op f x <|> return x
 where
  foldPAux p op f x = do
    v   <- p
    sep <- optionMaybe <| op
    case sep of
      Nothing -> return <| f v x
      _       -> foldlP p op f (f v x)

parseString :: String -> Maybe String -> IO (Either AbsError.Error (Diagram ParserInfo))
parseString str scope = first parseError <$> runParserT whileParser emptyState (fromMaybe "" scope) str
  where emptyState = ParserState {fileName = scope}

parseFile :: String -> IO (Either AbsError.Error (Diagram ParserInfo))
parseFile fileName = do
  input <- readFile fileName
  parseString input (Just fileName)

whileParser :: FlowParser (Diagram ParserInfo)
whileParser = whiteSpace >> diagram <* eof

rowInfo :: FlowParser ParserInfo
rowInfo = do
  position <- getPosition
  return ParserInfo {src = sourceName position, column = sourceColumn position, row = sourceLine position}

diagram :: FlowParser (Diagram ParserInfo)
diagram = do
  info <- rowInfo
  skipMany parseComment
  body <- many $ (whiteSpace <|> parseComment) *> flow <* (whiteSpace <|> parseComment)
  st   <- getState
  return <| DiagramRule info body

flow :: FlowParser (Flow ParserInfo)
flow =
  participant
    <|> parseGroup
    <|> parseHeader
    <|> parseNote
    <|> parseSetValue
    <|> parseInclude
    <|> parseDestroy
    <|> parseSingleRequest
    <|> parseLabeldGroup
    <|> parseDelay
    <|> parseTitle

parseTitle :: FlowParser (Flow ParserInfo)
parseTitle = do
  info <- rowInfo
  try <| string "title"
  v <- lettersTillDelimiter newline
  return <| Title info <| Text {t = lines v}

parseGroup :: FlowParser (Flow ParserInfo)
parseGroup = do
  info <- rowInfo
  try (string "group") <* whiteSpace
  string ":"
  label <- linedText <$> lettersTillDelimiter newline
  rs    <- manyTill (whiteSpace *> flow <* whiteSpace) (string "end group")
  whiteSpace
  return <| AbsFlow.Group info label rs

parseComment :: FlowParser ()
parseComment = do
  info <- rowInfo
  whiteSpace
  try (string "#")
  comment <- lettersTillDelimiter newline
  whiteSpace
  return ()

parseHeader :: FlowParser (Flow ParserInfo)
parseHeader = do
  info <- rowInfo
  try <| string "header for"
  participant <- lettersTillDelimiter newline
  string "data"
  assignments <- whiteSpace *> endBy assignment (char ';') <* whiteSpace
  string "end data"
  entityDef <- processEntity participant
  return (Header info <| Head entityDef assignments)

parseNote :: FlowParser (Flow ParserInfo)
parseNote = do
  info <- rowInfo
  try <| string "note"
  whiteSpace
  (position, entities, txt) <- parseNoteOver <|> parseNoteLeftRight
  return <| Note info position entities txt
 where
  noteParticipant v = processEntity =<< lettersTillDelimiter (void v)
  noteTxt = do
    commaSep <- optionMaybe <| char ':'
    linedText <$> case commaSep of
      (Just _) -> lettersTillDelimiter (void endOfLine)
      _        -> do
        linedTxt <- lettersTillDelimiter (string "end note")
        string "end note"
        return linedTxt
  parseNoteLeftRight = do
    notePosition <- choice [string "left" $> NoteLeft, string "right" $> NoteRight] <* whiteSpace
    string "of"
    entityDef <- noteParticipant <| choice [char ':', endOfLine]
    txt       <- noteTxt
    return (notePosition, [entityDef], txt)
  parseNoteOver = do
    try <| string "over" <* whiteSpace
    e   <- sepBy (noteParticipant <| choice [char ',', char ':', endOfLine]) (char ',')
    txt <- noteTxt
    return (NoteAbove, e, txt)

parseSetValue :: FlowParser (Flow ParserInfo)
parseSetValue = do
  info <- rowInfo
  try <| string "set"
  participant <- lettersTillDelimiter <| string "to"
  string "to" <* whiteSpace
  SetTo info participant <$> value

includeFile
  :: String -- ^ 
  -> M.Map String String -- ^ 
  -> ParserInfo -- ^ 
  -> FlowParser (Flow ParserInfo)
includeFile file subst importInfo = do
  state           <- getState
  importedDiagram <- liftIO <| parseFile file
  case importedDiagram of
    Right (DiagramRule _ flow) -> return <| Include importInfo file subst flow
    Left  e                    -> unexpected <| show e

parseInclude :: FlowParser (Flow ParserInfo)
parseInclude = do
  info <- rowInfo
  try <| string "include"
  p <- lettersTillDelimiter <| char '{'
  string "{" <* whiteSpace
  subst <- foldr (uncurry M.insert) M.empty <$> sepBy (substitution <| choice [separator, try <| char '}']) separator
  string "}" <* whiteSpace
  state           <- getState
  filePathToParse <- liftIO <| toAbsolute (fileName state) p
  includeFile filePathToParse subst info
 where
  toAbsolute :: Maybe FilePath -> FilePath -> IO FilePath
  toAbsolute scope path = if isRelative path then makeAbsolute (maybe "" dropFileName scope <> path) else return path
  separator :: FlowParser Char
  separator = char ','
  substitution :: FlowParser a -> FlowParser (String, String)
  substitution d = do
    e1 <- try <| lettersTillDelimiter (choice [char '/', char '}'])
    string "/"
    e2 <- lettersTillDelimiter d
    return (e1, e2)

participant :: FlowParser (Flow ParserInfo)
participant = do
  info <- rowInfo
  try (string "participant")
  p1        <- try <| lettersTillDelimiter (choice [char '[', char '\n'])
  path      <- C.optional (parseJsDefintion <* whiteSpace)
  entityDef <- processEntity p1
  return <| Participant info p1 path
 where
  parseJsDefintion = do
    try (string "[")
    p <- lettersTillDelimiter (char ']')
    string "]"
    return p

parseDestroy :: FlowParser (Flow ParserInfo)
parseDestroy = do
  info <- rowInfo
  try (string "destroy") <* whiteSpace
  e      <- lettersTillDelimiter newline
  entity <- processEntity e
  return <| Destroy info entity

parseDelay :: FlowParser (Flow ParserInfo)
parseDelay = do
  info <- rowInfo
  try (string "...")
  txt <- linedText <$> lettersTillDelimiter (string "...")
  string "..."
  return <| Delay info txt

parseLabeldGroup :: FlowParser (Flow ParserInfo)
parseLabeldGroup = do
  info               <- rowInfo
  (groupType, flows) <- alternativeGroup <|> optionalGroup <|> loopGroup
  return <| LabelledGroup info groupType flows

parseSingleRequest :: FlowParser (Flow ParserInfo)
parseSingleRequest = do
  info <- rowInfo
  reqs <- parseRequest
  return <| Requests info reqs

fromRequestParticipant :: FlowParser [Char]
fromRequestParticipant =
  lettersTillDelimiter (choice [void <| try (char '['), void <| try allArrows, void <| try (char '\n')])

jsEntity :: FlowParser String
jsEntity =
  do
      l  <- try letter
      ls <- many <| choice [alphaNum, char '.', char '_']
      return (l : ls)
    <|> return []

optionalGroup :: FlowParser (GroupType, [Labelling ParserInfo])
optionalGroup = do
  try <| string "opt"
  whiteSpace
  optional <- labeledGroupRequest "end"
  whiteSpace
  return (Optional, [optional])

loopGroup :: FlowParser (GroupType, [Labelling ParserInfo])
loopGroup = do
  try <| string "loop"
  whiteSpace
  loop <- labeledGroupRequest "end"
  whiteSpace
  return (Loop, [loop])

alternativeGroup :: FlowParser (GroupType, [Labelling ParserInfo])
alternativeGroup = do
  try <| string "alt"
  whiteSpace
  thenAlternative <- labeledGroupRequest "else"
  whiteSpace
  elseAlternative <- labeledGroupRequest "end"
  whiteSpace
  return (Alternative, [thenAlternative, elseAlternative])

labeledGroupRequest :: String -> FlowParser (Labelling ParserInfo)
labeledGroupRequest str = do
  info        <- rowInfo
  altLabel    <- linedText <$> lettersTillDelimiter newline
  altRequests <- manyTill (whiteSpace *> flow <* whiteSpace) (try (lookAhead <| string str)) <|> return []
  string str
  return <| Labelling info altLabel altRequests

parseRequest :: FlowParser (Request ParserInfo)
parseRequest =
  do
      requestInfo <- rowInfo
      r1          <- try <| requestUnit startBlockArrow
      requests    <- whiteSpace *> flowRequests <* whiteSpace
      r2          <- requestUnit closeBlockArrow
      return <| AbsFlow.Request requestInfo r1 requests (Just r2)
    <|> do
          requestInfo <- rowInfo
          r1          <- try <| requestUnit arrows
          return <| AbsFlow.Request requestInfo r1 [] Nothing
 where
  flowRequests :: FlowParser [Flow ParserInfo]
  flowRequests = manyTill (whiteSpace *> flow <* whiteSpace) (try (lookAhead <| startCloseArrow)) <|> return []
   where
    startCloseArrow = do
      fromRequestParticipant
      whiteSpace *> jsFunction
      closeBlockArrow

  requestUnit :: FlowParser Arrows -> FlowParser (BasicRequest ParserInfo)
  requestUnit arrow = do
    basicRequestInfo <- rowInfo
    p1               <- fromRequestParticipant
    js1              <- whiteSpace *> jsFunction
    arrow_ty         <- arrow
    p2               <- lettersTillDelimiter <| choice [void <| try (char '['), void <| try (char ':')]
    js2              <- whiteSpace *> jsFunction
    string ":"
    annotation_1 <- lettersTillDelimiter newline
    d            <- requestData <* whiteSpace
    entityDef1   <- processEntity p1
    entityDef2   <- processEntity p2
    return
      <| BasicRequest basicRequestInfo
                      (RequestEntity entityDef1 js1)
                      arrow_ty
                      (RequestEntity entityDef2 js2)
                      (linedText annotation_1)
                      d

jsFunction :: FlowParser (Maybe String)
jsFunction =
  do
      whiteSpace *> (try <| char '[')
      js <- whiteSpace *> jsEntity <* whiteSpace
      char ']' <* whiteSpace
      return <| Just js
    <|> return Nothing

requestData :: FlowParser (Maybe RequestData)
requestData =
  do
      try <| string "data" <* whiteSpace
      string "application/"
      d <- parseJson <|> parseFormEncoded
      string "end data"
      return (Just d)
    <|> return Nothing
 where
  parseFormEncoded :: FlowParser RequestData
  parseFormEncoded = do
    string "x-www-form-urlencoded"
    Form <$> (whiteSpace *> listAssignment <* whiteSpace)

  parseJson :: FlowParser RequestData
  parseJson = do
    try $ string "json"
    Json <$> (whiteSpace *> jsonValue <* whiteSpace)

allArrows :: FlowParser Arrows
allArrows = try openBlockArrow <|> try closeBlockArrow <|> try arrow

startBlockArrow :: FlowParser Arrows
startBlockArrow = try openNewParticipanBlockArrow <|> openBlockArrow

openNewParticipanBlockArrow :: FlowParser Arrows
openNewParticipanBlockArrow =
  do
      try <| string "-->*+"
      return <| NewParticipantBlockArrow Dash
    <|> do
          string "->*+"
          return <| NewParticipantBlockArrow Thick

openBlockArrow :: FlowParser Arrows
openBlockArrow =
  do
      try <| string "-->+"
      return <| NewBlockArrow Dash
    <|> do
          string "->+"
          return <| NewBlockArrow Thick

closeBlockArrow :: FlowParser Arrows
closeBlockArrow =
  do
      try <| string "-->-"
      return <| ClosingBlockArrow Dash
    <|> do
          try <| string "->-"
          return <| ClosingBlockArrow Thick
    <|> do
          try <| string "-X-"
          return <| FailedArrow Thick
    <|> do
          string "-X-"
          return <| FailedArrow Thick

arrows :: FlowParser Arrows
arrows = do
  a <- arrow
  notFollowedBy (char '-') <?> "participant definition"
  return a

arrow :: FlowParser Arrows
arrow =
  do
      try <| string "-->*"
      return (NewParticipantArrow Dash)
    <|> do
          try <| string "->*"
          return (NewParticipantArrow Thick)
    <|> do
          try <| string "-->"
          return (Arrow Dash)
    <|> do
          try <| string "->"
          return (Arrow Thick)
    <|> do
          try <| string "-X"
          return (FailedArrow Thick)
    <|> do
          string "--X"
          return (FailedArrow Dash)

identifier :: FlowParser Identifier
identifier = do
  c  <- upper
  cs <- f
  return <| Identifier (c : cs)
 where
  f =
    do
        v  <- try <| choice [upper, char '_']
        vs <- f
        return (v : vs)
      <|> return []

value :: FlowParser Value
value = StringValue <$> quotedString <|> (BoolValue <$> try bool) <|> IntegerValue <$> integer

separator = do
  whiteSpace
  char ','
  whiteSpace

float = rd <$> (plus <|> minus <|> number)
 where
  rd    = read :: String -> Float
  minus = (:) <$> char '-' <*> number

integer = rd <$> (plus <|> minus <|> number)
 where
  rd    = read :: String -> Integer
  minus = (:) <$> char '-' <*> number

bool :: FlowParser Bool
bool =
  do
      try <| string "true"
      return True
    <|> do
          string "false"
          return False

pair :: FlowParser (String, JsonValue)
pair = do
  whiteSpace
  p <- quotedString
  whiteSpace
  string ":"
  whiteSpace
  (p, ) <$> jsonValue

jsonValue :: FlowParser JsonValue
jsonValue =
  JsonString
    <$> try quotedString
    <|> JsonObject
    <$> try parseJsonObj
    <|> JsonArray
    <$> try parseArray
    <|> JsonInteger
    <$> try integer
    <|> JsonBoolean
    <$> bool
    <|> parseNull
    <|> JsonDouble
    <$> float
    <|> JsonIdentifier
    <$> identifier
 where
  parseNull = do
    try <| string "null"
    return JsonNullTk
  parseJsonObj = do
    try <| string "{"
    whiteSpace
    (j, _) <- foldlP pair (try separator) f (id, S.empty)
    whiteSpace
    string "}"
    return (j [])
    where f v@(key, _) (acc, mem) = if S.member key mem then (acc, mem) else (acc . (v :), S.insert key mem)
  parseArray = do
    string "["
    whiteSpace
    v <- sepBy jsonValue (try separator)
    whiteSpace
    string "]"
    return v


assignmentKey :: FlowParser Identifier
assignmentKey = do
  c  <- letter
  cs <- many <| satisfy (/= '=')
  return $ Identifier (c : cs)

listAssignment :: FlowParser [Assignment]
listAssignment =
  do
      try (lookAhead <| string "end data")
      return []
    <|> do
          a <- whiteSpace *> assignment
          string ";"
          as <- whiteSpace *> listAssignment
          return (a : as)

assignment :: FlowParser Assignment
assignment = do
  id_1 <- try assignmentKey
  whiteSpace
  char '='
  whiteSpace
  v <- AssignmentByValue id_1 <$> value <|> AssignmentById id_1 <$> identifier
  whiteSpace
  return v
