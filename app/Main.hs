import           System.Exit                    ( die
                                                , exitSuccess
                                                )
import           System.IO

import           AbsFlow
import           AbsParser
import           AbsSvg
import           Flow
import           Intermediate
import           IntermediateAbs
import           Print
import           PrintFlow
import           PrintSvg
import           Svg
import           Font
import           TTF
import           StaticAnalysis
import           System.Environment             ( getArgs
                                                , lookupEnv
                                                )


parser :: (Font a) => FontTable a -> EntityTable -> Diagram Info -> Either String String
parser font table absDiagram = case check table absDiagram of
  Right _ -> case draw font table absDiagram of
    Right intermediate -> Right (prettyPrint <| drawToSvg intermediate)
    Left  s            -> Left s
  Left e -> Left <| show e
 where
  drawToSvg :: Draw -> SVG
  drawToSvg = tranformTo

parserText :: (String -> IO ()) -> String -> String -> (EntityTable -> Diagram Info -> Either String String) -> IO ()
parserText error path dest_path tranformer = do
  absDiagram <- parseFile path
  case absDiagram of
    Left  s             -> error <| show s
    Right (table, tree) -> case tranformer table tree of
      Left  s   -> error s
      Right svg -> writeFile dest_path svg

parserStream :: (String -> IO ()) -> Maybe String -> (EntityTable -> Diagram Info -> Either String String) -> IO ()
parserStream error scope tranformer = do
  diagram    <- getContents
  absDiagram <- parseString diagram scope
  case absDiagram of
    Left  s             -> error <| show s
    Right (table, tree) -> case tranformer table tree of
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
      _ -> hPutStr stderr "Wrong transformer"
    _ -> hPutStr stderr "Wrong number of arguments"


f d = let fontFile = "/Users/gianluca/Desktop/projects/Static/Fonts/Monaco2.ttf"
          svgFile = "/Users/gianluca/Desktop/projects/Static/Diagrams/diagram/" ++ d ++ ".diagram"
          destFile = "/Users/gianluca/Desktop/projects/Static/Diagrams/svg/" ++ d ++ ".diagram.svg" 
      in  getTTFFontTable fontFile 14 >>= \input -> parserText die svgFile destFile (parser input)