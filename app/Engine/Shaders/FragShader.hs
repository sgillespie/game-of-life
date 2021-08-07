{-# LANGUAGE QuasiQuotes #-}
module Engine.Shaders.FragShader (fragShaderCode) where

import Data.ByteString (ByteString())

import Vulkan.Utils.ShaderQQ.HLSL.Shaderc (frag)

fragShaderCode :: ByteString
fragShaderCode
  = [frag|
      float4 main([[vk::location(0)]] const float3 col) : SV_TARGET
        {
            return float4(col, 1);
        }
    |]
