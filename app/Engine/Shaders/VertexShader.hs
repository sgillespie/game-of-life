{-# LANGUAGE QuasiQuotes #-}
module Engine.Shaders.VertexShader (vertexShaderCode) where

import Data.ByteString (ByteString())

import Vulkan.Utils.ShaderQQ.HLSL.Shaderc (vert)

vertexShaderCode :: ByteString
vertexShaderCode
  = [vert|
      struct VSInput {
        float2 position: POSITION;
        float3 color: COLOR;
      };

      struct VSOutput {
        float4 pos : SV_POSITION;
        [[vk::location(0)]] float3 col;
      };

      VSOutput main(const uint i : SV_VertexID, VSInput input) {
        VSOutput output;
        
        output.pos = float4(input.position, 0, 1.0);
        output.col = input.color;

        return output;
      }
    |]
