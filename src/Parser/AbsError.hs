module AbsError where 


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