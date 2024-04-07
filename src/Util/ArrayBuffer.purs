module Util.ArrayBuffer where

import Data.ArrayBuffer.Types (ArrayViewType)

class SizedArrayType :: ArrayViewType -> Constraint
class SizedArrayType a where
  byteLength :: Int

