module Engine.Domain.Frame (Frame(..), advanceFrame) where

import Data.Vector
import Data.Word

import Control.Monad.Trans.Resource
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_swapchain

-- |Per-frame resources
data Frame = Frame
  { fIndex :: Word64,
    fStartTime :: Double,
    fSurface :: SurfaceKHR,
    fSwapchain :: SwapchainKHR,
    fImageExtent :: Extent2D,
    fRenderPass :: RenderPass,
    fFramebuffers :: Vector Framebuffer,
    fPipelineLayout :: PipelineLayout,
    fGraphicsPipeline :: Pipeline,
    fImageAvailable :: Semaphore,
    fRenderFinished :: Semaphore,
    fVertexBuffer :: Buffer,
    fIndexBuffer :: Buffer,
    fResources :: (ReleaseKey, InternalState),
    fGpuWork :: Fence
  }

advanceFrame :: MonadResource m => Frame -> m Frame
advanceFrame f = return f { fIndex = succ (fIndex f) }
