module Util.Math.Transform where

import Data.FastVect.FastVect (Vect)

type Transform =
  { location :: Vect 3 Number
  , rotation :: Vect 4 Number
  , scale :: Vect 3 Number
  }
