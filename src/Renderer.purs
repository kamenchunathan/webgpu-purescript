module Renderer
  ( Renderer
  , createMeshBuffers
  , initializeRenderPipeline
  , createRenderer
  , render
  , shaderSource
  ) where

import Prelude

import Assets (Mesh, extractMeshData)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array as Array
import Data.ArrayBuffer.Typed as TypedArrayBuffer
import Data.ArrayBuffer.Types (Float32Array)
import Data.ArrayBuffer.Types as ArrayBuffer.Types
import Data.ArrayBuffer.ValueMapping (byteWidth)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.BufferSource (fromArrayBuffer)
import Web.GPU.GPUBlendFactor as GPUBlendFactor
import Web.GPU.GPUBlendOperation as GPUBlendOperation
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUBufferUsage as GPUBufferUsage
import Web.GPU.GPUCanvasContext (GPUCanvasContext, getCurrentTexture)
import Web.GPU.GPUColor (gpuColorRGBA)
import Web.GPU.GPUColorWrite as GPUColorWrite
import Web.GPU.GPUCommandEncoder (beginRenderPass, finish)
import Web.GPU.GPUCullMode as GPUCullMode
import Web.GPU.GPUDevice (GPUDevice, createBuffer, createCommandEncoder, createPipelineLayout, createRenderPipeline, createShaderModule, queue)
import Web.GPU.GPUFrontFace as GPUFrontFace
import Web.GPU.GPUIndexFormat as GPUIndexFormat
import Web.GPU.GPULoadOp as GPULoadOp
import Web.GPU.GPUPrimitiveTopology as GPUPrimitiveTopology
import Web.GPU.GPUQueue (submit, writeBuffer)
import Web.GPU.GPURenderPassEncoder (drawIndexed, setIndexBuffer, setVertexBuffer, end, setPipeline)
import Web.GPU.GPURenderPipeline (GPURenderPipeline)
import Web.GPU.GPUStoreOp as GPUStoreOp
import Web.GPU.GPUTexture (createView)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUVertexFormat as GPUVertexFormat
import Web.GPU.Internal.Bitwise ((.|.))
import Web.GPU.Internal.RequiredAndOptional as RequiredAndOptional
import World (World, extractEntitiesFromWorld)

foreign import shaderSource :: String

newtype Renderer = Renderer
  { renderContext :: GPUCanvasContext
  , device :: GPUDevice
  , renderTextureFormat :: GPUTextureFormat
  , renderPipeline :: GPURenderPipeline
  }

createRenderer :: GPUCanvasContext -> GPUDevice -> GPUTextureFormat -> GPURenderPipeline -> Renderer
createRenderer renderContext device renderTextureFormat renderPipeline =
  Renderer
    { renderContext
    , device
    , renderTextureFormat
    , renderPipeline
    }

initializeRenderPipeline :: GPUDevice -> GPUTextureFormat -> Effect GPURenderPipeline
initializeRenderPipeline device renderTextureFormat = do
  shader <- liftEffect $ createShaderModule
    device
    ( RequiredAndOptional.requiredAndOptional
        { code: shaderSource }
        { label: "Default Shader" }
    )

  renderPipelineLayout <- createPipelineLayout
    device
    ( RequiredAndOptional.requiredAndOptional
        { bindGroupLayouts: [] }
        { label: "simple Pipeline Layout" }
    )

  createRenderPipeline
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
        { label: "Simple Render Pipeline"
        , fragment:
            ( RequiredAndOptional.r
                { entryPoint: "fs_main"
                , module: shader
                , targets:
                    [ ( RequiredAndOptional.requiredAndOptional
                          { format: renderTextureFormat }
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

render
  :: Renderer
  -> World
  -> Effect Unit
render (renderer@(Renderer { device, renderContext, renderPipeline })) world = do
  entities <-  extractEntitiesFromWorld world

  -- TODO(nathan): Currently only rendering one single entity 
  entity <- liftMaybe (error "World is empty, currently needs entities to render") (Array.head entities)
  { vertexBuffer, indexBuffer } <- createMeshBuffers renderer entity.mesh
  let { indices, vertices } = extractMeshData entity.mesh

  -- render queue
  queue <- queue device
  writeBuffer queue vertexBuffer 0 (fromArrayBuffer vertices)
  writeBuffer queue indexBuffer 0 (fromArrayBuffer indices)
  do
    i :: Float32Array <- TypedArrayBuffer.whole vertices
    Console.log $ unsafeCoerce i
  texture <- getCurrentTexture renderContext
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
                { clearValue: gpuColorRGBA 0.0 0.79 0.75 1.0 }
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

createMeshBuffers
  :: Renderer
  -> Mesh
  -> Effect { indexBuffer :: GPUBuffer, vertexBuffer :: GPUBuffer }
createMeshBuffers (Renderer renderer) mesh = do
  vertexBuffer <- liftEffect $ createBuffer
    renderer.device
    ( RequiredAndOptional.requiredAndOptional
        { size: mesh.positionBufView.byteLength
        , usage: GPUBufferUsage.index .|. GPUBufferUsage.copyDst .|. GPUBufferUsage.vertex
        }
        { label: "vertex buffer" }
    )
  indexBuffer <- liftEffect $ createBuffer
    renderer.device
    ( RequiredAndOptional.requiredAndOptional
        { size: mesh.indexBufView.byteLength
        , usage: GPUBufferUsage.index .|. GPUBufferUsage.copyDst
        }
        { label: "index buffer" }
    )
  pure { vertexBuffer, indexBuffer }
