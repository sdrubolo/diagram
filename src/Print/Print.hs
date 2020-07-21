module Print where

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

prettyPrint :: Print a => a -> String
prettyPrint s = foldr ($) "" $ prt 0 s []

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Float where
  prt _ x = doc (shows x)

instance Print Bool where
  prt _ True = doc (showString "true")
  prt _ False = doc (showString "false")

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'       -> showString "\\\\"
  '\n'       -> showString "\\n"
  '\t'       -> showString "\\t"
  _          -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

justShow :: Int -> String -> Print.Doc
justShow i a = indentation i . doc (showString a) where indentation i = concatD $ replicate i $ doc (showString "\t")
