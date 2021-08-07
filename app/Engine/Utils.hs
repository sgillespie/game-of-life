module Engine.Utils where

import Control.Exception
import Control.Monad.IO.Class
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import qualified Data.Vector as V

import Control.Monad.Trans.Resource
import Vulkan.CStruct.Extends
import Vulkan.Core10 hiding (withBuffer)
import Vulkan.Zero
import VulkanMemoryAllocator

import Engine.Monad

copyBuffer
  :: Buffer
  -> Buffer
  -> DeviceSize
  -> Vulkan ()
copyBuffer src dst bufferSize = withOneTimeCommands $ do
  let copy = BufferCopy
        { srcOffset = zero,
          dstOffset = zero,
          size = bufferSize
        }
  
  commandBuffer <- getCommandBuffer
  cmdCopyBuffer commandBuffer src dst [copy]

noSuchThing :: MonadIO m => String -> m a
noSuchThing message = liftIO $ throwIO err
  where err = IOError Nothing NoSuchThing "" message Nothing Nothing

oneSecond :: Num a => a
oneSecond = 1e9

unwrapM2 :: Monad m => (m a, m b) -> m (a, b)
unwrapM2 (a, b) = do
  a' <- a
  b' <- b
  return (a', b')

withBuffer'
  :: MonadResource m
  => DeviceSize
  -> BufferUsageFlags
  -> MemoryPropertyFlags
  -> Allocator
  -> m (Buffer, ReleaseKey, Allocation)
withBuffer' bufferSize usageFlags requiredMemoryFlags allocator = do
  let bufferInfo = BufferCreateInfo
        { next = (),
          flags = zero,
          size = fromIntegral bufferSize,
          usage = usageFlags,
          sharingMode = SHARING_MODE_EXCLUSIVE,
          queueFamilyIndices = []
        }

      allocInfo = zero { requiredFlags = requiredMemoryFlags }

  (bufferRelease, result) <- withBuffer allocator bufferInfo allocInfo allocate
  let (buffer, alloc, _) = result
  
  return (buffer, bufferRelease, alloc)

withImageView' :: MonadResource m => Device -> Image -> Format -> m ImageView
withImageView' device image format
  = snd <$> withImageView device viewInfo Nothing allocate
  where components = ComponentMapping
          { r = COMPONENT_SWIZZLE_IDENTITY,
            g = COMPONENT_SWIZZLE_IDENTITY,
            b = COMPONENT_SWIZZLE_IDENTITY,
            a = COMPONENT_SWIZZLE_IDENTITY
          }

        subRange = ImageSubresourceRange
          { aspectMask = IMAGE_ASPECT_COLOR_BIT,
            baseMipLevel = 0,
            levelCount = 1,
            baseArrayLayer = 0,
            layerCount = 1
          }

        viewInfo = ImageViewCreateInfo
          { next = (),
            flags = zero,
            image = image,
            viewType = IMAGE_VIEW_TYPE_2D,
            format = format,
            components = components,
            subresourceRange = subRange
          }

withOneTimeCommands :: CmdT Vulkan a -> Vulkan a
withOneTimeCommands commands = do
  commandPool <- getCommandPool
  device <- getDevice
  graphicsQueue <- getGraphicsQueue
  
  -- Create a command buffer
  let commandBufferInfo = CommandBufferAllocateInfo
        { commandPool = commandPool,
          level = COMMAND_BUFFER_LEVEL_PRIMARY,
          commandBufferCount = 1
        }

  (releaseCommandBuffers, commandBuffers) <-
    withCommandBuffers device commandBufferInfo allocate
  let commandBuffer = V.head commandBuffers

  -- Record the command buffer
  let beginInfo = CommandBufferBeginInfo
        { next = (),
          flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
          inheritanceInfo = Nothing
        }

  res <- runCmdT commandBuffer beginInfo commands
  
  -- Submit the command buffer
  let submitInfo = SubmitInfo
        { next = (),
          waitSemaphores = [],
          waitDstStageMask = [],
          commandBuffers = [commandBufferHandle commandBuffer],
          signalSemaphores = []
        }
  queueSubmit graphicsQueue [SomeStruct submitInfo] zero
  queueWaitIdle graphicsQueue
  release releaseCommandBuffers

  return res

