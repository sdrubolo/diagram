import           System.Exit                    ( die
                                                , exitSuccess
                                                )
import           System.IO

import           AbsFlow
import           AbsParser
import           AbsSvg
import           Flow
--import           Intermediate
import           IntermediateAbs
import           Print
import           PrintFlow
import           PrintSvg
import           Svg
import           Font
import           TTF
import           Analyzer
import           System.Environment             ( getArgs
                                                , lookupEnv
                                                )


parser :: (Font a) => FontTable a -> Diagram ParserInfo -> Either String String
parser font absDiagram = Right "" -- case check absDiagram of
  -- Right _ -> undefined --case draw font table absDiagram of
    --Right intermediate -> Right (prettyPrint <| drawToSvg intermediate)
    --Left  s            -> Left s
  -- Left  e -> Left <| show e
 -- where
  -- drawToSvg :: Draw -> SVG
  -- drawToSvg = tranformTo

parserText :: (String -> IO ()) -> String -> String -> (Diagram ParserInfo -> Either String String) -> IO ()
parserText error path dest_path tranformer = do
  absDiagram <- parseFile path
  case absDiagram of
    Left  s    -> error <| show s
    Right tree -> case tranformer tree of
      Left  s   -> error s
      Right svg -> writeFile dest_path svg

parserStream :: (String -> IO ()) -> Maybe String -> (Diagram ParserInfo -> Either String String) -> IO ()
parserStream error scope tranformer = do
  diagram    <- getContents
  absDiagram <- parseString diagram scope
  case absDiagram of
    Left  s    -> error <| show s
    Right tree -> case tranformer tree of
      Left  s   -> error s
      Right svg -> putStrLn svg


getTTFFontTable :: String -> Float -> IO (FontTable TTF)
getTTFFontTable = getFont

main = do
  args <- getArgs
  case args of
    [fontFile] -> do
      scope <- lookupEnv "path"
      input <- getTTFFontTable fontFile 14
      parserStream die scope (parser input)
    [fontFile, svgFile, destFile] -> do
      input <- getTTFFontTable fontFile 14
      parserText die svgFile destFile (parser input)
    [tranformer, fontFile, svgFile, destFile] -> case tranformer of
      "svg" -> do
        input <- getTTFFontTable fontFile 14
        parserText die svgFile destFile (parser input)
    _ -> hPutStr stderr "Wrong number of arguments"
