module Camera where

import Data.FastVect.FastVect (Vect)

type PerspectiveCamera =
  { transform ::
      { position :: Vect 3 Number
      , scale :: Vect 3 Number
      }
  }
