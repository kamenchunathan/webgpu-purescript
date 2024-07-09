module World
  ( World
  , addToWorld
  , emptyWorld
  , extractEntitiesFromWorld
  ) where

import Prelude

import Assets (Mesh)
import Control.Monad.ST.Global (Global, toEffect)
import Data.Array.ST (STArray, freeze)
import Data.Array.ST as STArray
import Effect (Effect)
import Util.Math.Transform (Transform)

type Entity = { transform :: Transform, mesh :: Mesh }

newtype World = World (STArray Global Entity)

emptyWorld :: Effect World
emptyWorld = toEffect $ World <$> STArray.new

addToWorld :: World -> Entity -> Effect Unit
addToWorld (World entityArray) entity = do
  _ <- toEffect $ STArray.push entity entityArray
  pure unit

extractEntitiesFromWorld :: World -> Effect (Array Entity)
extractEntitiesFromWorld (World entities) = do
  toEffect $ freeze entities
