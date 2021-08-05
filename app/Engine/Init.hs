module Engine.Init (withVulkanHandles) where

import Control.Monad.Trans.Resource
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface

import Engine.Monad

withVulkanHandles
  :: (MonadResource m, MonadThrow m)
  => Instance
  -> SurfaceKHR
  -> m VulkanHandles
withVulkanHandles = undefined
