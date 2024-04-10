module Assets (loadMesh, defaultTriangleMesh, Mesh) where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as Web
import Control.Monad.Error.Class (liftEither)
import Data.Argonaut ((.:))
import Data.Argonaut as Argonaut
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as Decode
import Data.Argonaut.Decode as Deocde
import Data.Array as Array
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.ArrayBuffer.Cast as ArrayBuffer.Cast
import Data.ArrayBuffer.DataView as ArrayBuffer.Dataview
import Data.ArrayBuffer.Typed as ArrayBuffer.Typed
import Data.ArrayBuffer.Types
  ( ArrayBuffer
  , ArrayView
  , Float32Array
  , Uint16Array
  , Uint8
  )
import Data.Either (Either(..), either, note)
import Data.Float32 as Float32
import Data.Function (applyFlipped)
import Data.Maybe (Maybe(..))
import Data.UInt as UInt
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)

infixl 1 applyFlipped as |>

type Mesh =
  { buffer ::
      { byteLength :: Int
      , bytes :: ArrayBuffer
      }
  , positionBufView ::
      { byteLength :: Int
      , byteOffset :: Int
      }
  , indexBufView ::
      { byteLength :: Int
      , byteOffset :: Int
      }

  }

-- Result of the initial parsing before fetching the buffers which are separate
type PreDataMesh =
  { buffer ::
      { byteLength :: Int
      , uri :: String
      }
  , positionBufView ::
      { byteLength :: Int
      , byteOffset :: Int
      }
  , indexBufView ::
      { byteLength :: Int
      , byteOffset :: Int
      }

  }

loadMesh :: String -> Aff Mesh
loadMesh uri = do
  gltfMeshResp <-
    Web.get
      ResponseFormat.json
      ("/assets/meshes/" <> uri)
      >>= liftEither <<< (either (Left <<< Aff.error <<< Affjax.printError) Right)

  mesh <- decodeMeshGltf gltfMeshResp.body
    |> either (Left <<< Aff.error <<< Decode.printJsonDecodeError) Right
    |> liftEither

  bufResp <-
    Web.get
      ResponseFormat.arrayBuffer
      ("/assets/meshes/" <> mesh.buffer.uri)
      >>= liftEither <<< (either (Left <<< Aff.error <<< Affjax.printError) Right)

  pure
    { buffer:
        { byteLength: mesh.buffer.byteLength
        , bytes: bufResp.body
        }
    , positionBufView: mesh.positionBufView
    , indexBufView: mesh.indexBufView
    }

decodeMeshGltf
  :: DecodeJson
       PreDataMesh
  => Argonaut.Json
  -> Either Decode.JsonDecodeError
       PreDataMesh
decodeMeshGltf mesh = do
  s <- Deocde.decodeJson mesh
  sqMesh <- s .: "meshes" >>=
    ( \primitives ->
        Array.head primitives |> note (Decode.AtIndex 0 Decode.MissingValue)
    )
  firstPrim <- sqMesh .: "primitives" >>=
    ( \primitives ->
        Array.head primitives |> note (Decode.AtIndex 0 Decode.MissingValue)
    )
  position <- firstPrim .: "attributes" >>= (_ .: "POSITION")
  posAccessor :: { bufferView :: Int } <- s .: "accessors" >>=
    ( \accessors ->
        Array.index accessors position |> note (Decode.AtIndex 0 Decode.MissingValue)
    )
  posBufferView <- s .: "bufferViews" >>=
    ( \viewIndex ->
        Array.index viewIndex posAccessor.bufferView |> note (Decode.AtIndex 0 Decode.MissingValue)
    )
  byteLength <- posBufferView .: "byteLength"
  byteOffset <- posBufferView .: "byteOffset"

  buffer <- s .: "buffers" >>=
    ( \bufViews ->
        Array.index bufViews posAccessor.bufferView |> note (Decode.AtIndex 0 Decode.MissingValue)
    )

  -- Index buffers
  indices <- firstPrim .: "indices"
  indexAccessor :: { bufferView :: Int } <- s .: "accessors" >>=
    ( \accessors ->
        Array.index accessors indices |> note (Decode.AtIndex 0 Decode.MissingValue)
    )
  indexBufView <- s .: "bufferViews" >>=
    ( \viewIndex ->
        Array.index viewIndex indexAccessor.bufferView |> note (Decode.AtIndex 0 Decode.MissingValue)
    )

  pure
    { buffer
    , positionBufView:
        { byteLength
        , byteOffset
        }
    , indexBufView
    }

defaultTriangleMesh :: Aff Mesh
defaultTriangleMesh = do
  -- do 
  --   squareMesh  <- loadMesh "square.gltf" 
  --   squareMeshArrayView :: (ArrayView Float32) <- ArrayBuffer.Typed.whole squareMesh.buffer.bytes |> liftEffect 
  --   squareVertexPositions <- ArrayBuffer.Typed.toArray squareMeshArrayView |> liftEffect 
  --   liftEffect $ Console.log $ unsafeCoerce squareVertexPositions
  let
    vertexPositions = Float32.fromNumber' <$>
      [ -0.25
      , 0.5
      , 0.0
      , -0.5
      , -0.5
      , 0.0
      , 0.0
      , -0.5
      , 0.0
      , 0.25
      , 0.5
      , 0.0
      , 0.5
      , -0.5
      , 0.0
      ]

    indices = UInt.fromInt <$> [ 0, 1, 2, 3, 2, 4 ]

  vPosView :: Float32Array <- ArrayBuffer.Typed.fromArray vertexPositions
    |> liftEffect
  indexView :: Uint16Array <- ArrayBuffer.Typed.fromArray indices
    |> liftEffect

  let vPosByteLen = ArrayBuffer.Typed.byteLength vPosView
  let indexByteLen = ArrayBuffer.Typed.byteLength indexView

  -- Create an empty buffer to which we will append the values in the other array buffers
  bytes <- ArrayBuffer.empty (vPosByteLen + indexByteLen) |> liftEffect
  -- get an arrayview as uint8 so we can write to it using a typed api
  bufView :: ArrayView Uint8 <- ArrayBuffer.Typed.whole bytes |> liftEffect

  -- Cast each array to uint8 and use set to copy their values to the main array
  vPosUint8 <- vPosView
    |> ArrayBuffer.Typed.buffer
    |> ArrayBuffer.Dataview.whole
    |> ArrayBuffer.Cast.toUint8Array
    >>= ArrayBuffer.Typed.toArray
    |> liftEffect
  _ <- ArrayBuffer.Typed.set bufView Nothing vPosUint8 |> liftEffect

  indexUint8 <- indexView
    |> ArrayBuffer.Typed.buffer
    |> ArrayBuffer.Dataview.whole
    |> ArrayBuffer.Cast.toUint8Array
    >>= ArrayBuffer.Typed.toArray
    |> liftEffect
  _ <- ArrayBuffer.Typed.set bufView (Just vPosByteLen) indexUint8 |> liftEffect

  -- Console.log (unsafeCoerce bytes) |> liftEffect
  pure
    { buffer:
        { byteLength: ArrayBuffer.byteLength bytes
        , bytes
        }
    , positionBufView:
        { byteLength: vPosByteLen
        , byteOffset: 0
        }
    , indexBufView:
        { byteLength: indexByteLen
        , byteOffset: vPosByteLen
        }

    }
