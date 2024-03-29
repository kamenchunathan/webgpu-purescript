module Main (main) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Error, error, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Promise.Aff as Promise.Aff
import Web.DOM.NonElementParentNode (getElementById)
import Web.GPU.GPU (GPU, getPreferredCanvasFormat, requestAdapter)
import Web.GPU.GPUAdapter (GPUAdapter, requestDevice)
import Web.GPU.GPUCanvasContext (configure)
import Web.GPU.GPUDevice (GPUDevice)
import Web.GPU.GPUPowerPreference as GPUPowerPreference
import Web.GPU.HTMLCanvasElement (getContext)
import Web.GPU.Internal.RequiredAndOptional as RequiredAndOptional
import Web.GPU.Navigator as Navigator
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document, navigator)

main ∷ Effect Unit
main = launchAff_ do
  { gpu, device } <- setup
  mCanvas <- liftEffect $ getCanvas
  canvas <- liftMaybe (error "Unable to Get Canvas element") mCanvas
  configureCanvas gpu device canvas

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

  mGPUAdapter <- liftAff $
    ( liftEffect $ Promise.Aff.toAff <$> requestAdapter gpu
        ( RequiredAndOptional.o
            { powerPreference: GPUPowerPreference.highPerformance }
        )
    ) >>= identity
  adapter <- liftMaybe (error "WebGPU not supported on this platform") mGPUAdapter

  mDevice <- liftAff $
    ( liftEffect $ Promise.Aff.toAff <$> requestDevice adapter
        ( RequiredAndOptional.o
            { label: "Default device" }
        )
    ) >>= identity
  device <- liftMaybe (error "WebGPU not supported on this platform") mDevice
  pure { gpu, adapter, device }

getCanvas :: Effect (Maybe HTMLCanvasElement)
getCanvas = do
  doc <- window >>= document
  canvasElem <- getElementById "canvas" $ toNonElementParentNode doc
  pure $ canvasElem >>= HTMLCanvasElement.fromElement

configureCanvas
  :: forall m
   . MonadEffect m
  => MonadThrow Error m
  => GPU
  -> GPUDevice
  -> HTMLCanvasElement
  -> m Unit
configureCanvas gpu device canvas = do
  mCtx <- liftEffect $ getContext canvas
  ctx <- liftMaybe (error "Unable to Get Canvas Context") mCtx
  format <- liftEffect $ getPreferredCanvasFormat gpu
  liftEffect $ configure ctx (RequiredAndOptional.r { device, format })
