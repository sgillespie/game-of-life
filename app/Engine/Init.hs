module Engine.Init
  ( withInstance',
    withVulkanHandles
  ) where

import Control.Monad.IO.Class
import Data.Bits
import Data.List
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Control.Monad.Trans.Resource
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Requirement
import Vulkan.Version
import Vulkan.Utils.Initialization
import Vulkan.Zero
import VulkanMemoryAllocator hiding (getPhysicalDeviceProperties)

import Engine.Monad
import Engine.Utils

withInstance' :: MonadResource m => V.Vector B.ByteString -> m Instance
withInstance' requiredExts = do
  let 
      applicationInfo' = ApplicationInfo
        { applicationName = Just "Gillespie's Game of Life",
          applicationVersion = MAKE_API_VERSION 0 1 0,
          engineName = Just "Seangine+",
          engineVersion = MAKE_API_VERSION 0 1 0,
          apiVersion = API_VERSION_1_0
        }

      instanceCreateInfo = zero
        { applicationInfo = Just applicationInfo' }

      req = flip (RequireInstanceExtension Nothing) minBound
          <$> V.toList requiredExts
            

  instance' <- createDebugInstanceFromRequirements req [] instanceCreateInfo

  return instance'

withVulkanHandles
  :: (MonadResource m, MonadThrow m)
  => Instance
  -> SurfaceKHR
  -> m VulkanHandles
withVulkanHandles instance' surface = do
  physicalDevice <- choosePhysicalDevice instance'
  (graphicsFamilyIndex, presentFamilyIndex)
    <- getQueueFamilyIndices physicalDevice surface
  device <- withDevice' physicalDevice [graphicsFamilyIndex, presentFamilyIndex]
  allocator <- withAllocator' instance' physicalDevice device
  graphicsQueue <- getDeviceQueue device graphicsFamilyIndex 0
  presentQueue <- getDeviceQueue device presentFamilyIndex 0
  commandPool <- withCommandPool' device graphicsFamilyIndex

  return VulkanHandles
    { vhInstance = instance',
      vhPhysicalDevice = physicalDevice,
      vhDevice = device,
      vhAllocator = allocator,
      vhGraphicsQueue = graphicsQueue,
      vhGraphicsQueueFamily = graphicsFamilyIndex,
      vhPresentQueue = presentQueue,
      vhPresentQueueFamily = presentFamilyIndex,
      vhCommandPool = commandPool
    }

choosePhysicalDevice
  :: MonadIO m
  => Instance
  -> m PhysicalDevice
choosePhysicalDevice instance' = do
  -- Look for a device with Swapchain support
  let isDeviceSuitable device = do
        (_, exts) <- enumerateDeviceExtensionProperties device Nothing
        let swapchainSupport
              = V.any ((KHR_SWAPCHAIN_EXTENSION_NAME ==) . extensionName) exts

        if swapchainSupport
          then Just <$> getPhysicalDeviceProperties device
          else pure Nothing
  
  -- Score devices
  let score :: PhysicalDeviceProperties -> Int
      score dev = case deviceType dev of
        PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> 10
        PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 5
        PHYSICAL_DEVICE_TYPE_CPU -> 2
        PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> 1
        _ -> 0

  device <- pickPhysicalDevice instance' isDeviceSuitable score
  case device of
    Just (_, device') -> pure device'
    Nothing -> noSuchThing "Failed to find a suitable physical device!"

getQueueFamilyIndices :: MonadIO m => PhysicalDevice -> SurfaceKHR -> m (Word32, Word32)
getQueueFamilyIndices physicalDevice surface = do
  families <- getPhysicalDeviceQueueFamilyProperties physicalDevice

  let indexed = V.indexed families

      isGraphicsFamily fam = queueFlags fam .&. QUEUE_GRAPHICS_BIT /= zeroBits
      isPresentationFamily device idx = getPhysicalDeviceSurfaceSupportKHR device idx surface

      graphicsFamilyIndex
        = V.head $ fromIntegral . fst
          <$> V.filter (isGraphicsFamily . snd) indexed

  presentFamilyIndex
      <- V.head <$> V.filterM
           (isPresentationFamily physicalDevice)
           (V.generate (V.length families) fromIntegral)
 
  return (graphicsFamilyIndex, presentFamilyIndex)

withDevice'
  :: MonadResource m
  => PhysicalDevice
  -> [Word32]
  -> m Device
withDevice' physicalDevice queueIndices = do
  let createInfos = fmap
        (SomeStruct . createQueueInfo)
        (nub queueIndices)

      deviceCreateInfo = DeviceCreateInfo
        { next = (),
          flags = zero,
          queueCreateInfos = V.fromList createInfos,
          enabledLayerNames = ["VK_LAYER_KHRONOS_validation"],
          enabledExtensionNames = [KHR_SWAPCHAIN_EXTENSION_NAME],
          enabledFeatures = Just features
        }

      createQueueInfo idx = DeviceQueueCreateInfo () zero idx [1]

      features = zero { samplerAnisotropy = True }

  (_, device) <- withDevice physicalDevice deviceCreateInfo Nothing allocate
  return device

withAllocator' :: MonadResource m => Instance -> PhysicalDevice -> Device -> m Allocator
withAllocator' instance' physicalDevice device
  = snd <$> withAllocator allocInfo allocate
  where allocInfo = AllocatorCreateInfo
          { flags = zero,
            physicalDevice = physicalDeviceHandle physicalDevice,
            device = deviceHandle device,
            preferredLargeHeapBlockSize = zero,
            allocationCallbacks = Nothing,
            deviceMemoryCallbacks = Nothing,
            frameInUseCount = zero,
            heapSizeLimit = zero,
            vulkanFunctions = Nothing,
            recordSettings = Nothing,
            instance' = instanceHandle instance',
            vulkanApiVersion = API_VERSION_1_0
          }

withCommandPool' :: MonadResource m => Device -> Word32 -> m CommandPool
withCommandPool' device queueFamilyIndex
  = snd <$> withCommandPool device commandPoolInfo Nothing allocate
  where commandPoolInfo = CommandPoolCreateInfo
          { flags = zero,
            queueFamilyIndex = queueFamilyIndex
          }
