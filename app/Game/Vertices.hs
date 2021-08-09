module Game.Vertices (vertices, vertexIndices) where

import Foreign.C.Types
import Linear
import Engine.Domain

vertices :: [Vertex]
vertices
  = [ Vertex (V2 (-0.5) (-0.5)) (V3 1 1 1),
      Vertex (V2   0.5  (-0.5)) (V3 1 1 1),
      Vertex (V2   0.5    0.5)  (V3 1 1 1),
      Vertex (V2 (-0.5)   0.5)  (V3 1 1 1)
    ]

vertexIndices :: [CUShort]
vertexIndices = [0, 1, 1, 2, 2, 3, 3, 0]
