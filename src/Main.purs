module Main where

import Prelude

import Assets (defaultTriangleMesh)
import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Data.FastVect.Common (term)
import Data.FastVect.FastVect (replicate)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Error, error, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Promise.Aff as Promise.Aff
import Renderer (createRenderer, initializeRenderPipeline, render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.GPU.GPU (GPU, getPreferredCanvasFormat, requestAdapter)
import Web.GPU.GPUAdapter (GPUAdapter, requestDevice)
import Web.GPU.GPUCanvasContext (configure)
import Web.GPU.GPUDevice (GPUDevice)
import Web.GPU.GPUPowerPreference as GPUPowerPreference
import Web.GPU.GPUTextureUsage as GPUTextureUsage
import Web.GPU.HTMLCanvasElement (getContext)
import Web.GPU.Internal.RequiredAndOptional as RequiredAndOptional
import Web.GPU.Navigator as Navigator
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document, navigator)
import World (addToWorld, emptyWorld)

main ∷ Effect Unit
main = launchAff_ run

run :: forall m. MonadAff m => MonadThrow Error m => m Unit
run = do
  -- Set up for rendering
  { gpu, device } <- setup
  renderTextureFormat <- liftEffect $ getPreferredCanvasFormat gpu

  -- Get and configure context
  canvas <- liftEffect $ getCanvas
    >>= liftMaybe (error "Unable to Get Canvas element")
  ctx <- liftEffect $ getContext canvas
    >>= liftMaybe (error "Unable to Get Canvas Context")
  liftEffect $
    configure
      ctx
      ( RequiredAndOptional.requiredAndOptional
          { device, format: renderTextureFormat }
          { usage: GPUTextureUsage.renderAttachment }
      )

  -- Create a renderer
  renderPipeline <- liftEffect $ initializeRenderPipeline device renderTextureFormat
  let renderer = createRenderer ctx device renderTextureFormat renderPipeline

  -- Populate world with one item
  triangleMesh <- liftAff $ defaultTriangleMesh
  world <- liftEffect emptyWorld
  liftEffect $ addToWorld world
    { mesh: triangleMesh
    , transform:
        { location: replicate term 0.0
        , rotation: replicate term 0.0
        , scale: replicate term 0.0
        }
    }

  -- Render
  liftEffect $ render renderer world 

setup
  :: forall m
   . MonadEffect m
  => MonadThrow Error m
  => MonadAff m
  => m
       { gpu :: GPU
       , adapter :: GPUAdapter
       , device :: GPUDevice
       }
setup = do
  mGPU <- liftEffect $ window >>= navigator >>= Navigator.gpu
  gpu <- liftMaybe (error "WebGPU not supported on this platform") mGPU

  mGPUAdapter <- liftAff
    $ join
    $ liftEffect
    $ Promise.Aff.toAff <$> requestAdapter gpu
        ( RequiredAndOptional.o
            { powerPreference: GPUPowerPreference.highPerformance }
        )
  adapter <- liftMaybe (error "WebGPU not supported on this platform") mGPUAdapter

  mDevice <- liftAff
    $ join
    $ liftEffect
    $ Promise.Aff.toAff <$> requestDevice adapter
        ( RequiredAndOptional.o
            { label: "Default device" }
        )
  device <- liftMaybe (error "WebGPU not supported on this platform") mDevice

  pure { gpu, adapter, device }

getCanvas :: Effect (Maybe HTMLCanvasElement)
getCanvas = do
  doc <- window >>= document
  canvasElem <- getElementById "canvas" $ toNonElementParentNode doc
  pure $ canvasElem >>= HTMLCanvasElement.fromElement

