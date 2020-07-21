module IntermediateTranform where

import           IntermediateAbs

class TranformTo a b where
  tranformTo :: a -> b
