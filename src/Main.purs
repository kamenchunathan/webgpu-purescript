module Main where

import Prelude

import Assets (defaultTriangleMesh, loadMesh)
import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.ArrayBuffer.Typed (whole)
import Data.ArrayBuffer.Typed as TypedArrayBuffer
import Data.ArrayBuffer.Types (ArrayBuffer, Float32Array, Uint16Array)
import Data.ArrayBuffer.Types as ArrayBuffer.Types
import Data.ArrayBuffer.ValueMapping (byteWidth)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Error, error, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Promise.Aff as Promise.Aff
import Renderer (shaderSource)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Util ((|>))
import Web.DOM.NonElementParentNode (getElementById)
import Web.GPU.BufferSource (fromArrayBuffer)
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
import Web.GPU.GPUIndexFormat as GPUIndexFormat
import Web.GPU.GPULoadOp as GPULoadOp
import Web.GPU.GPUPowerPreference as GPUPowerPreference
import Web.GPU.GPUPrimitiveTopology as GPUPrimitiveTopology
import Web.GPU.GPUQueue (submit, writeBuffer)
import Web.GPU.GPURenderPassEncoder (drawIndexed, end, setIndexBuffer, setPipeline, setVertexBuffer)
import Web.GPU.GPURenderPipeline (GPURenderPipeline)
import Web.GPU.GPUStoreOp as GPUStoreOp
import Web.GPU.GPUTexture (createView)
import Web.GPU.GPUTextureUsage as GPUTextureUsage
import Web.GPU.GPUVertexFormat as GPUVertexFormat
import Web.GPU.HTMLCanvasElement (getContext)
import Web.GPU.Internal.Bitwise ((.|.))
import Web.GPU.Internal.RequiredAndOptional as RequiredAndOptional
import Web.GPU.Navigator as Navigator
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document, navigator)

main ∷ Effect Unit
main = launchAff_ do
  renderApp

renderApp :: forall m. MonadAff m => MonadThrow Error m => m Unit
renderApp = do
  -- Set up for rendering
  { gpu, device } <- setup
  canvas <- liftEffect $ getCanvas
    >>= liftMaybe (error "Unable to Get Canvas element")

  configureCanvas gpu device canvas
  ctx <- liftEffect $ getContext canvas
    >>= liftMaybe (error "Unable to Get Canvas Context")

  -- load assets to render
  squareMesh <- liftAff $  loadMesh "square.gltf"
  let
    vertexBuffer = ArrayBuffer.slice
      squareMesh.positionBufView.byteOffset
      (squareMesh.positionBufView.byteOffset + squareMesh.positionBufView.byteLength)
      squareMesh.buffer.bytes
    indexBuffer = ArrayBuffer.slice
      squareMesh.indexBufView.byteOffset
      (squareMesh.indexBufView.byteOffset + squareMesh.indexBufView.byteLength)
      squareMesh.buffer.bytes

  vertexArrayView :: Float32Array <- liftEffect $ whole vertexBuffer
  indexArrayView :: Uint16Array <- liftEffect $ whole indexBuffer

  -- Render
  { renderPipeline, vertexBuf, indexBuf } <-
    next
      gpu
      device
      { vertexBufferLen: TypedArrayBuffer.byteLength vertexArrayView
      , indexBufferLen: TypedArrayBuffer.byteLength indexArrayView
      }
      |> liftEffect
  pure unit
  render ctx device renderPipeline
    { vertexBuffer: vertexBuf
    , vertices: vertexBuffer
    , indexBuffer: indexBuf
    , indices: indexBuffer
    } |> liftEffect

next
  :: forall m
   . MonadEffect m
  => GPU
  -> GPUDevice
  -> { vertexBufferLen :: Int, indexBufferLen :: Int }
  -> m
       { renderPipeline :: GPURenderPipeline
       , vertexBuf :: GPUBuffer
       , indexBuf :: GPUBuffer
       }
next gpu device { vertexBufferLen, indexBufferLen } = do
  preferredTextureFormat <- liftEffect $ getPreferredCanvasFormat gpu
  shader <- liftEffect $ createShaderModule
    device
    ( RequiredAndOptional.requiredAndOptional
        { code: shaderSource }
        { label: "Default Shader" }
    )

  renderPipelineLayout <- liftEffect $ createPipelineLayout
    device
    ( RequiredAndOptional.requiredAndOptional
        { bindGroupLayouts: [] }
        { label: "simple-pipeline-layout" }
    )

  renderPipeline <- liftEffect $ createRenderPipeline
    device
    ( RequiredAndOptional.requiredAndOptional
        { layout: renderPipelineLayout
        , vertex:
            ( RequiredAndOptional.requiredAndOptional
                { module: shader, entryPoint: "vs_main" }
                { buffers:
                    [ ( RequiredAndOptional.r
                          { arrayStride: 3 * byteWidth (Proxy :: _ ArrayBuffer.Types.Float32)
                          , attributes:
                              [ RequiredAndOptional.r
                                  { format: GPUVertexFormat.float32x3
                                  , offset: 0
                                  , shaderLocation: 0
                                  }
                              ]
                          }
                      )
                    ]
                }
            )
        }
        { label: "simple-render-pipeline"
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
  vertexBuf <- liftEffect $ createBuffer
    device
    ( RequiredAndOptional.requiredAndOptional
        { size: vertexBufferLen
        , usage: GPUBufferUsage.index .|. GPUBufferUsage.copyDst .|. GPUBufferUsage.vertex
        }
        { label: "vertex buffer" }
    )
  indexBuf <- liftEffect $ createBuffer
    device
    ( RequiredAndOptional.requiredAndOptional
        { size: indexBufferLen
        , usage: GPUBufferUsage.index .|. GPUBufferUsage.copyDst
        }
        { label: "index buffer" }
    )
  pure { renderPipeline, vertexBuf, indexBuf }

render
  :: GPUCanvasContext
  -> GPUDevice
  -> GPURenderPipeline
  -> { indexBuffer :: GPUBuffer
     , indices :: ArrayBuffer
     , vertexBuffer :: GPUBuffer
     , vertices :: ArrayBuffer
     }
  -> Effect Unit
render ctx device renderPipeline { vertexBuffer, vertices, indexBuffer, indices } = do
  queue <- queue device
  writeBuffer queue vertexBuffer 0 (fromArrayBuffer vertices)
  writeBuffer queue indexBuffer 0 (fromArrayBuffer indices)
  do
    i :: Float32Array <- TypedArrayBuffer.whole vertices
    Console.log $ unsafeCoerce  i
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
                , storeOp: GPUStoreOp.store
                }
                { clearValue: gpuColorRGBA 1.0 0.79 0.75 1.0 }
            ]
        }
        { label: "renderpass-encoder" }
    )
  setPipeline renderPass renderPipeline
  setVertexBuffer renderPass 0 vertexBuffer
  setIndexBuffer renderPass indexBuffer GPUIndexFormat.uint16
  drawIndexed renderPass 6
  end renderPass
  commandBuf <- finish commandEncoder
  submit queue [ commandBuf ]

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
