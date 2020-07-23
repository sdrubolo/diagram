module AbsParser
  ( parseString
  , parseFile
  , blockComment
  , lettersTillDelimiter
  , lettersTillDelimiterNoBackTrim
  , parseTillDelimiter
  )
where

import           AbsData
import           AbsFlow
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

data ParserState = ParserState
  { fileName :: Maybe String
  , substMap :: M.Map String String
  , entityTable :: M.Map String EntityDef
  , entities :: [EntityName]
  , blocks :: [String]
  } deriving (Show)

type FlowParser a = ParsecT String ParserState IO a

parseError error =
  let position = errorPos error
  in  AbsFlow.Error
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
processEntity entity = do
  let trimEntity = trim entity
  entityToProcess <- M.findWithDefault trimEntity trimEntity . substMap <$> getState
  updateState (addEntityToState entityToProcess)
  return entityToProcess

addEntityToState :: String -> ParserState -> ParserState
addEntityToState e t@ParserState {..} = if M.member e entityTable
  then t
  else
    let (old, newTable) =
          M.insertLookupWithKey (\_ _ o -> o) e EntityDef {name = e, text = linedText e, prevs = entities} entityTable
    in  ParserState
          { entityTable = newTable
          , entities    = if e `L.elem` entities then entities else entities <> [e]
          , blocks      = blocks
          , fileName    = fileName
          , substMap    = substMap
          }

removeEscapedNewLine []                = []
removeEscapedNewLine ('\\' : 'n' : xs) = '\n' : removeEscapedNewLine xs
removeEscapedNewLine (x          : xs) = x : removeEscapedNewLine xs

lineComment = try (string "//") *> manyTill anyChar (void newline <|> void endOfLine)

blockComment = void <| try (string "/*") *> manyTill anyChar (try <| string "*/")

simpleWhitespace = void <| many1 (oneOf " \t\n")
inlineWhiteSpace = void <| many1 (oneOf " \t")

inLineWhiteSpace :: FlowParser ()
inLineWhiteSpace = choice [void <| many1 (oneOf " \t") *> inLineWhiteSpace, blockComment *> inLineWhiteSpace, return ()]

whiteSpace :: FlowParser ()
whiteSpace = choice [simpleWhitespace *> whiteSpace, lineComment *> whiteSpace, blockComment *> whiteSpace, return ()]

comments = choice [inlineWhiteSpace *> comments, lineComment *> comments, blockComment *> comments, return ()]

quotedString = between (char '"') (char '"') (many quotedStringChar)
 where
  quotedStringChar = escapedChar <|> normalChar
  escapedChar      = char '\\' *> oneOf ['\\', '"']
  normalChar       = noneOf "\""

plus = char '+' *> number

number = many1 digit

lettersTillDelimiter p = inLineWhiteSpace *> (removeEscapedNewLine . trim <$> parseTillDelimiter anyChar p) <* whiteSpace

lettersTillDelimiterNoBackTrim p =
  inLineWhiteSpace *> (removeEscapedNewLine . trim <$> parseTillDelimiter anyChar p) <* comments

parseTillDelimiter :: FlowParser a1 -> FlowParser a2 -> FlowParser [a1]
parseTillDelimiter s p = f
 where
  f =
    do
        (try <| lookAhead <| void p) <|> (try <| lookAhead <| void (string "//"))
        return []
      <|> do
            (try <| lookAhead <| void (string "/*"))
            blockComment *> f
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

parseString :: String -> Maybe String -> IO (Either AbsFlow.Error (EntityTable, Diagram Info))
parseString str scope = first parseError <$> runParserT whileParser emptyState "" str
  where emptyState = ParserState {entityTable = M.empty, blocks = [], entities = [], fileName = scope, substMap = M.empty}

parseFile :: String -> IO (Either AbsFlow.Error (EntityTable, Diagram Info))
parseFile fileName = first parseError <$> parseFileAux emptyState fileName
 where
  emptyState = ParserState {entityTable = M.empty, blocks = [], entities = [], fileName = Just fileName, substMap = M.empty}

parseFileAux :: ParserState -> String -> IO (Either ParseError (EntityTable, Diagram Info))
parseFileAux state fileName = do
  input <- readFile fileName
  runParserT whileParser (state { fileName = Just fileName }) fileName input

whileParser :: FlowParser (EntityTable, Diagram Info)
whileParser = whiteSpace >> diagram <* eof

rowInfo :: FlowParser Info
rowInfo = do
  position <- getPosition
  return Info {src = sourceName position, column = sourceColumn position, row = sourceLine position}

diagram :: FlowParser (EntityTable, Diagram Info)
diagram = do
  info <- rowInfo
  body <- f
  st   <- getState
  let tableInfo = EntityTable {entitiesMap = entityTable st, allEntities = entities st}
  return <| (tableInfo, DiagramRule info body)
 where
  f =
    do
        fw  <- whiteSpace *> flow <* whiteSpace
        fws <- f
        return (fw : fws)
      <|> return []

flow :: FlowParser (Flow Info)
flow =
  participant
    <|> parseGroup
    <|> parseHeader
    <|> parseNote
    <|> parseSetValue
    <|> parseInclude
    <|> parseDestroy
    <|> parseSingleRequest
    <|> parseDelay
    <|> parseTitle

parseTitle = do
  info <- rowInfo
  try <| string "title"
  v <- lettersTillDelimiter newline
  return <| Title info <| linedText v

header :: FlowParser Head
header = do
  string "header for"
  participant <- lettersTillDelimiter newline
  string "data"
  assignments <- whiteSpace *> endBy assignment (char ';') <* whiteSpace
  string "end data"
  entityDef <- processEntity participant
  return <| Head entityDef assignments

parseGroup :: FlowParser (Flow Info)
parseGroup = do
  info <- rowInfo
  try (string "group") <* whiteSpace
  string ":"
  label <- linedText <$> lettersTillDelimiter newline
  rs    <- manyTill (whiteSpace *> flow <* whiteSpace) (string "end group")
  whiteSpace
  return <| AbsFlow.Group info label rs

parseHeader :: FlowParser (Flow Info)
parseHeader = do
  info <- rowInfo
  try (lookAhead (string "header for"))
  Header info <$> header

parseNote :: FlowParser (Flow Info)
parseNote = do
  info <- rowInfo
  try <| string "note"
  whiteSpace
  note <- parseNoteOver <|> parseNoteLeftRight
  return <| uncurry3 (Note info) note
 where
  noteParticipant v = processEntity =<< lettersTillDelimiterNoBackTrim (void v)
  noteTxt = do
    commaSep <- optionMaybe <| char ':'
    linedText <$> case commaSep of
      (Just _) -> lettersTillDelimiter (void endOfLine)
      _        -> do
        linedTxt <- lettersTillDelimiter (string "end note")
        string "end note"
        return linedTxt
  parseNoteLeftRight = do
    notePosition <- choice [string "left", string "right"] <* whiteSpace
    string "of"
    entityDef <- noteParticipant <| choice [char ':', endOfLine]
    txt       <- noteTxt
    return (if notePosition == "left" then NoteLeft else NoteRigth, [entityDef], txt)
  parseNoteOver = do
    try <| string "over" <* whiteSpace
    e   <- sepBy (noteParticipant <| choice [char ',', char ':', endOfLine]) (char ',')
    txt <- noteTxt
    return (NoteAbove, e, txt)

parseSetValue = do
  info <- rowInfo
  try <| string "set"
  participant <- lettersTillDelimiter <| string "to"
  string "to" <* whiteSpace
  SetTo info participant <$> value

includeFile file subst importInfo = do
  state           <- getState
  filePathToParse <- liftIO <| toAbsolute (fileName state) file
  importedDiagram <- liftIO <| parseFileAux (state { substMap = subst }) filePathToParse
  case importedDiagram of
    Right (info, DiagramRule _ flow) -> do
      mapM_ (\e -> updateState <| addEntityToState e) (allEntities info L.\\ entities state)
      return <| Include importInfo file subst flow
    Left e -> unexpected <| show e
  where toAbsolute scope path = if isRelative path then makeAbsolute (maybe "" dropFileName scope <> path) else return path

parseInclude = do
  info <- rowInfo
  try <| string "include"
  p <- lettersTillDelimiter <| char '{'
  string "{" <* whiteSpace
  subst <- foldr (uncurry M.insert) M.empty <$> sepBy (substitution <| choice [separator, try <| char '}']) separator
  string "}" <* whiteSpace
  includeFile p subst info
 where
  separator = char ','
  substitution :: FlowParser a -> FlowParser (String, String)
  substitution d = do
    e1 <- try <| lettersTillDelimiter (choice [char '/', char '}'])
    string "/"
    e2 <- lettersTillDelimiter d
    return (e1, e2)

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

parseDestroy = do
  info <- rowInfo
  try (string "destroy") <* whiteSpace
  e      <- lettersTillDelimiter newline
  entity <- processEntity e
  return <| Destroy info entity

parseDelay = do
  info <- rowInfo
  try (string "...")
  txt <- linedText <$> lettersTillDelimiter (string "...")
  string "..."
  return <| Delay info txt

parseSingleRequest = do
  info <- rowInfo
  reqs <- request
  return <| Requests info reqs

fromRequestParticipant =
  lettersTillDelimiter (choice [void <| try (char '['), void <| try allArrows, void <| try (char '\n')])

flowRequests :: FlowParser [Flow Info]
flowRequests = manyTill (whiteSpace *> flow <* whiteSpace) (try (lookAhead <| startCloseArrow)) <|> return []
 where
  startCloseArrow = do
    fromRequestParticipant
    whiteSpace *> jsFunction
    closeBlockArrow

jsEntity :: FlowParser String
jsEntity =
  do
      l  <- try letter
      ls <- many <| choice [alphaNum, char '.', char '_']
      return (l : ls)
    <|> return []

request :: FlowParser (Request Info)
request = alternativeRequest <|> optionalRequest <|> loopRequest <|> parseRequest

optionalRequest :: FlowParser (Request Info)
optionalRequest = do
  alternativeInfo <- rowInfo
  try <| string "opt"
  whiteSpace
  optional <- labeledGroupRequest "end"
  whiteSpace
  return <| GroupRequest alternativeInfo Optional [optional]

loopRequest :: FlowParser (Request Info)
loopRequest = do
  alternativeInfo <- rowInfo
  try <| string "loop"
  whiteSpace
  loop <- labeledGroupRequest "end"
  whiteSpace
  return <| GroupRequest alternativeInfo Loop [loop]

alternativeRequest :: FlowParser (Request Info)
alternativeRequest = do
  alternativeInfo <- rowInfo
  try <| string "alt"
  whiteSpace
  thenAlternative <- labeledGroupRequest "else"
  whiteSpace
  elseAlternative <- labeledGroupRequest "end"
  whiteSpace
  return <| GroupRequest alternativeInfo Alternative [thenAlternative, elseAlternative]

labeledGroupRequest str = do
  altInfo     <- rowInfo
  altLabel    <- linedText <$> lettersTillDelimiter newline
  altRequests <- manyTill (whiteSpace *> flow <* whiteSpace) (try (lookAhead <| string str)) <|> return []
  string str
  return <| LabeledGroupRequest altInfo altLabel altRequests

parseRequest :: FlowParser (Request Info)
parseRequest =
  do
      requestInfo <- rowInfo
      r1          <- try <| requestUnit openBlockArrow
      requests    <- whiteSpace *> flowRequests <* whiteSpace
      r2          <- requestUnit closeBlockArrow
      return <| AbsFlow.Request requestInfo r1 requests (Just r2)
    <|> do
          requestInfo <- rowInfo
          r1          <- try <| requestUnit arrows
          return <| AbsFlow.Request requestInfo r1 [] Nothing

requestUnit :: FlowParser Arrows -> FlowParser (BasicRequest Info)
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
      d <- parseJson <|> parseFormEncoded
      string "end data"
      return (Just d)
    <|> return Nothing
 where
  parseFormEncoded = do
    string "application/x-www-form-urlencoded"
    Form <$> (whiteSpace *> listAssignment <* whiteSpace)
  parseJson = do
    try $ string "application/json"
    Json <$> (whiteSpace *> jsonValue <* whiteSpace)

allArrows = try openBlockArrow <|> try closeBlockArrow <|> try arrow

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
