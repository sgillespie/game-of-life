module Engine.Render (renderFrame, runCommandBuffers) where

import Control.Monad.Trans.Reader
import qualified Data.Vector as V

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource
import Vulkan.CStruct.Extends
import Vulkan.Core10 hiding (withMappedMemory)
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero

import Engine.GraphicsPipeline
import Engine.Monad
import Engine.Utils

renderFrame :: V.Vector CommandBuffer -> VulkanFrame ()
renderFrame commandBuffers = do
  device <- getDevice
  graphicsQueue <- getGraphicsQueue
  presentQueue <- getPresentQueue
  Frame{..} <- getFrame

  (_, imageIndex) <- acquireNextImageKHR device fSwapchain maxBound fImageAvailable zero

  let submitInfo = SomeStruct $ SubmitInfo
        { next = (),
          waitSemaphores = [fImageAvailable],
          waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT],
          commandBuffers = [handle],
          signalSemaphores = [fRenderFinished]
        }
      commandBuffer = commandBuffers V.! fromIntegral imageIndex
      handle = commandBufferHandle commandBuffer
      
      presentInfo = PresentInfoKHR
        { next = (),
          waitSemaphores = [fRenderFinished],
          swapchains = [fSwapchain],
          imageIndices = [imageIndex],
          results = zero
        }

  queueSubmit graphicsQueue [submitInfo] fGpuWork
  _ <- queuePresentKHR presentQueue presentInfo

  -- Do not proceed until the previous frame has finished
  fResult <- waitForFencesSafe device [fGpuWork] True oneSecond
  case fResult of
    SUCCESS -> return ()
    err -> liftIO $ throwM $ userError (show err)
  resetFences device [fGpuWork]
  
  return ()

runCommandBuffers :: MonadUnliftIO m => Frame -> V.Vector CommandBuffer -> m ()
runCommandBuffers frame' commandBuffers = do
  let beginInfo = CommandBufferBeginInfo
          { next = (),
            flags = zero,
            inheritanceInfo = Nothing
          }
      framebuffers = fFramebuffers frame'

  V.forM_ (V.zip commandBuffers framebuffers) $ \(commandBuffer, framebuffer) ->
    runCmdT commandBuffer beginInfo $ 
        recordCommandBuffer frame' framebuffer

recordCommandBuffer :: MonadUnliftIO m => Frame -> Framebuffer -> CmdT m ()
recordCommandBuffer Frame{..} framebuffer = do
  commandBuffer <- CmdT ask

  let renderPassInfo = RenderPassBeginInfo
          { next = (),
            renderPass = fRenderPass,
            framebuffer = framebuffer,
            renderArea = Rect2D zero fImageExtent,
            clearValues = [Color $ Float32 0.0 0.0 0.0 0]
          }
  
  cmdUseRenderPass commandBuffer renderPassInfo SUBPASS_CONTENTS_INLINE $ do
      cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS fGraphicsPipeline
      cmdBindVertexBuffers commandBuffer 0 [fVertexBuffer] [0]
      cmdBindIndexBuffer commandBuffer fIndexBuffer 0 INDEX_TYPE_UINT16

      cmdDrawIndexed commandBuffer (fromIntegral $ length vertexIndices) 1 0 0 0
