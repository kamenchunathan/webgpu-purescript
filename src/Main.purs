module Main (main) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Data.Array (length)
import Data.ArrayBuffer.Typed (fromArray)
import Data.ArrayBuffer.Types as ArrayBuffer.Types
import Data.ArrayBuffer.ValueMapping (byteWidth)
import Data.Float32 (Float32, fromNumber')
import Web.GPU.Internal.Bitwise ((.|.))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Error, error, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Promise.Aff as Promise.Aff
import Shader (shaderSource)
import Type.Prelude (Proxy(..))
import Web.DOM.NonElementParentNode (getElementById)
import Web.GPU.BufferSource (fromFloat32Array)
import Web.GPU.GPU (GPU, getPreferredCanvasFormat, requestAdapter)
import Web.GPU.GPUAdapter (GPUAdapter, requestDevice)
import Web.GPU.GPUBlendFactor as GPUBlendFactor
import Web.GPU.GPUBlendOperation as GPUBlendOperation
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUBufferUsage as GPUBufferUsage
import Web.GPU.GPUCanvasContext (GPUCanvasContext, configure, getCurrentTexture)
import Web.GPU.GPUColor (gpuColorRGBA)
import Web.GPU.GPUColorWrite as GPUColorWrite
import Web.GPU.GPUCommandEncoder (beginRenderPass, finish)
import Web.GPU.GPUCullMode as GPUCullMode
import Web.GPU.GPUDevice (GPUDevice, createBuffer, createCommandEncoder, createPipelineLayout, createRenderPipeline, createShaderModule, queue)
import Web.GPU.GPUFrontFace as GPUFrontFace
import Web.GPU.GPULoadOp as GPULoadOp
import Web.GPU.GPUPowerPreference as GPUPowerPreference
import Web.GPU.GPUPrimitiveTopology as GPUPrimitiveTopology
import Web.GPU.GPUQueue (submit, writeBuffer)
import Web.GPU.GPURenderPassEncoder (draw, end, setPipeline, setVertexBuffer)
import Web.GPU.GPURenderPipeline (GPURenderPipeline)
import Web.GPU.GPUStoreOp as GPUStoreOp
import Web.GPU.GPUTexture (createView)
import Web.GPU.GPUTextureUsage as GPUTextureUsage
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
  { renderPipeline, buf } <- liftEffect $ next gpu device
  -- TODO: temporary. remove this
  mCtx <- liftEffect $ getContext canvas
  ctx <- liftMaybe (error "Unable to Get Canvas Context") mCtx
  liftEffect $ render ctx device renderPipeline buf

next
  :: forall m
   . MonadEffect m
  => GPU
  -> GPUDevice
  -> m
       { renderPipeline :: GPURenderPipeline
       , buf :: GPUBuffer
       }
next gpu device = do
  preferredTextureFormat <- liftEffect $ getPreferredCanvasFormat gpu
  shader <- liftEffect $ createShaderModule
    device
    ( RequiredAndOptional.requiredAndOptional
        { code: shaderSource }
        { label: "Default Shader" }
    )

  renderPipelineLayout <- liftEffect $ createPipelineLayout
    device
    (RequiredAndOptional.r { bindGroupLayouts: [] })

  renderPipeline <- liftEffect $ createRenderPipeline
    device
    ( RequiredAndOptional.requiredAndOptional
        { layout: renderPipelineLayout
        , vertex: (RequiredAndOptional.r { module: shader, entryPoint: "vs_main" })
        }
        { label: "Hardcoded Render Pipeline"
        , fragment:
            ( RequiredAndOptional.r
                { entryPoint: "fs_main"
                , module: shader
                , targets:
                    [ ( RequiredAndOptional.requiredAndOptional
                          { format: preferredTextureFormat }
                          { blend: RequiredAndOptional.r
                              { color: RequiredAndOptional.o
                                  { operation: GPUBlendOperation.add
                                  , srcFactor: GPUBlendFactor.one
                                  , dstFactor: GPUBlendFactor.zero
                                  }
                              , alpha: RequiredAndOptional.o
                                  { operation: GPUBlendOperation.add
                                  , srcFactor: GPUBlendFactor.one
                                  , dstFactor: GPUBlendFactor.zero
                                  }
                              }
                          , writeMask: GPUColorWrite.all
                          }
                      )
                    ]
                }
            )
        , primitive: RequiredAndOptional.o
            { topology: GPUPrimitiveTopology.triangleList
            , frontFace: GPUFrontFace.ccw
            , cullMode: GPUCullMode.back
            , unclippedDepth: false
            }
        }
    )
  buf <- liftEffect $ createBuffer
    device
    ( RequiredAndOptional.requiredAndOptional
        { size: length vertices * byteWidth (Proxy :: _ ArrayBuffer.Types.Float32)
        , usage: GPUBufferUsage.vertex .|. GPUBufferUsage.copyDst
        }
        { label: "Triangles" }
    )
  pure { renderPipeline, buf }

render :: GPUCanvasContext -> GPUDevice -> GPURenderPipeline -> GPUBuffer -> Effect Unit
render ctx device renderPipeline buf = do
  queue <- queue device
  v <- fromArray vertices
  writeBuffer queue buf 0 (fromFloat32Array v)
  texture <- getCurrentTexture ctx
  view <- createView texture
  commandEncoder <- createCommandEncoder
    device
    (RequiredAndOptional.o { label: "command-encoder" })
  renderPass <- beginRenderPass
    commandEncoder
    ( RequiredAndOptional.requiredAndOptional
        { colorAttachments:
            [ RequiredAndOptional.requiredAndOptional
                { view
                , loadOp: GPULoadOp.clear
                , storeOp: GPUStoreOp.discard
                }
                { clearValue: gpuColorRGBA 1.0 0.79 0.75 1.0 }
            ]
        }
        { label: "renderpass-encoder" }
    )
  setPipeline renderPass renderPipeline
  -- setVertexBuffer renderPass 0 buf
  -- draw renderPass 3
  end renderPass
  commandBuf <- finish commandEncoder
  submit queue [ {- commandBuf -} ]

vertices :: Array Float32
vertices = fromNumber' <$>
  [ -0.25
  , 0.5
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , -0.5
  , -0.5
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , -0.5
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.25
  , 0.5
  , 0.0
  , 0.0
  , 1.0
  , 1.0
  , 0.0
  , -0.5
  , 0.0
  , 1.0
  , 0.0
  , 1.0
  , 0.5
  , -0.5
  , 0.0
  , 1.0
  , 1.0
  , 0.0
  ]

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
  liftEffect $
    configure
      ctx
      ( RequiredAndOptional.requiredAndOptional
          { device, format }
          { usage: GPUTextureUsage.renderAttachment }
      )
