module Engine.Domain.Vertex (Vertex(..), vertexAttributes) where

import Foreign.Ptr
import Foreign.Storable

import Linear
import Vulkan.Core10
  ( VertexInputAttributeDescription(..),
    Format(..)
  )
import Vulkan.Zero

-- |A vertex, containing a 2D position and RGB color
data Vertex = Vertex
  { position :: V2 Float,
    color :: V3 Float
  } deriving (Eq, Show)

instance Zero Vertex where
  zero = Vertex (V2 0 0) (V3 0 0 0)

instance Storable Vertex where
  sizeOf _
    = sizeOf (undefined :: V2 Float)
    + sizeOf (undefined :: V3 Float)
    + sizeOf (undefined :: V2 Float)
  alignment _ = alignment (undefined :: Float)

  -- TODO[sgillespie]: This is obviously wrong
  peek ptr = Vertex <$> peek ptr' <*> peek ptr'
    where ptr' = castPtr ptr

  poke ptr (Vertex pos col)
    = poke ptr' pos
    >> pokeByteOff ptr' (sizeOf pos) col
    where ptr' = castPtr ptr

vertexAttributes :: [VertexInputAttributeDescription]
vertexAttributes 
  = [ VertexInputAttributeDescription
        { location = 0,
          binding = 0,
          format = FORMAT_R32G32_SFLOAT,
          offset = 0
        },

      VertexInputAttributeDescription
        { location = 1,
          binding = 0,
          format = FORMAT_R32G32B32_SFLOAT,
          offset = fromIntegral $ sizeOf (undefined :: V2 Float)
        }
    ]
