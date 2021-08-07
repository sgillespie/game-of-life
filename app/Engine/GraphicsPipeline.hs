module Engine.GraphicsPipeline
  ( withVulkanFrame,
    withCommandBuffers'
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Bits
import Data.Traversable
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Clock (getMonotonicTime)
import qualified Data.Vector as V

import Control.Monad.Trans.Resource
import Vulkan.CStruct.Extends
import Vulkan.Core10 hiding (alignment, withImage, withMappedMemory)
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Zero
import VulkanMemoryAllocator hiding (getPhysicalDeviceProperties)

import Engine.Domain
import Engine.Monad
import Engine.Shaders
import Engine.Utils

withVulkanFrame :: SurfaceKHR -> [Vertex] -> [CUShort] -> Vulkan Frame
withVulkanFrame surface vertices vertexIndices = do
  device <- getDevice
  handles <- Vulkan ask
  allocator <- getAllocator
  
  (swapchain, extent) <- withSwapchain handles surface
  renderPass <- withRenderPass' device
  setLayout <- withDescriptorSetLayout' device
  pipelineLayout <- withPipelineLayout' device setLayout
  pipeline <- withGraphicsPipeline pipelineLayout renderPass extent
  imageViews <- withImageViews device swapchain
  framebuffers <- withFramebuffers device imageViews renderPass extent
  (available, finished) <- withSemaphores device
  resources <- allocate createInternalState closeInternalState
  start <- liftIO getMonotonicTime
  fence <- withFence' device
  vertexBuffer <- withVertexBuffer vertices allocator
  indexBuffer <- withIndexBuffer vertexIndices allocator

  return Frame
    { fIndex = 0,
      fStartTime = start,
      fSurface = surface,
      fSwapchain = swapchain,
      fImageExtent = extent,
      fRenderPass = renderPass,
      fFramebuffers = framebuffers,
      fPipelineLayout = pipelineLayout,
      fGraphicsPipeline = pipeline,
      fImageAvailable = available,
      fRenderFinished = finished,
      fVertexBuffer = vertexBuffer,
      fIndexBuffer = indexBuffer,
      fResources = resources,
      fGpuWork = fence
    }

withCommandBuffers' :: Frame -> Vulkan (V.Vector CommandBuffer)
withCommandBuffers' Frame{..} = do
  commandPool <- getCommandPool
  device <- getDevice
  
  let commandBufferInfo = CommandBufferAllocateInfo
        { commandPool = commandPool ,
          level = COMMAND_BUFFER_LEVEL_PRIMARY,
          commandBufferCount = fromIntegral $ V.length fFramebuffers }
  (_, commandBuffers) <- withCommandBuffers device commandBufferInfo allocate


  return commandBuffers

withSwapchain
  :: MonadResource m
  => VulkanHandles
  -> SurfaceKHR
  -> m (SwapchainKHR, Extent2D)
withSwapchain VulkanHandles{..} surface = do
  SurfaceCapabilitiesKHR{..} <-
    getPhysicalDeviceSurfaceCapabilitiesKHR vhPhysicalDevice surface

  let (sharingMode, famIndices) = if vhGraphicsQueueFamily == vhPresentQueueFamily
        then (SHARING_MODE_EXCLUSIVE, [])
        else (SHARING_MODE_CONCURRENT, [vhGraphicsQueueFamily, vhPresentQueueFamily])

      (Extent2D width height) = currentExtent
      extent = if width == maxBound && height == maxBound
          then Extent2D 800 600
          else currentExtent

      createInfo = SwapchainCreateInfoKHR
          { next = (),
            flags = zero,
            surface = surface,
            minImageCount = minImageCount + 1,
            imageFormat = FORMAT_B8G8R8A8_SRGB,
            imageColorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR,
            imageExtent = extent,
            imageArrayLayers = 1,
            imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
            imageSharingMode = sharingMode,
            queueFamilyIndices = famIndices,
            preTransform = currentTransform,
            compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
            presentMode = PRESENT_MODE_MAILBOX_KHR,
            clipped = True,
            oldSwapchain = zero
          }
  
  (_, swapchain) <- withSwapchainKHR vhDevice createInfo Nothing allocate
  return (swapchain, extent)

withRenderPass' :: MonadResource m => Device -> m RenderPass
withRenderPass' device = snd <$> withRenderPass device renderPassInfo Nothing allocate
  where renderPassInfo = RenderPassCreateInfo
          { next = (),
            flags = zero,
            attachments = [attachment],
            subpasses = [subpass],
            dependencies = [subpassDependency]
          }

        attachment = AttachmentDescription
          { flags = zero,
            format = FORMAT_B8G8R8A8_SRGB,
            samples = SAMPLE_COUNT_1_BIT,
            loadOp = ATTACHMENT_LOAD_OP_CLEAR,
            storeOp = ATTACHMENT_STORE_OP_STORE,
            stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE,
            stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE,
            initialLayout = IMAGE_LAYOUT_UNDEFINED,
            finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
          }

        subpass = SubpassDescription
          { flags = zero,
            pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS,
            inputAttachments = [],
            colorAttachments = [colorAttachment],
            resolveAttachments = [],
            depthStencilAttachment = Nothing,
            preserveAttachments = []
          }

        subpassDependency = SubpassDependency
          { srcSubpass = SUBPASS_EXTERNAL,
            dstSubpass = 0,
            srcStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            srcAccessMask = zero,
            dstStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            dstAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
            dependencyFlags = zero
          }

        colorAttachment = AttachmentReference
          { attachment = 0,
            layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          }

withGraphicsPipeline :: PipelineLayout -> RenderPass -> Extent2D -> Vulkan Pipeline
withGraphicsPipeline pipelineLayout renderPass extent@(Extent2D width height) = do
  device <- getDevice
  
  (fragShaderModule, vertShaderModule) <- withShaderModules device
  
  let pipelineCache = zero
  
      pipelineInfo = GraphicsPipelineCreateInfo
        { next = (),
          flags = zero,
          stages = [vertexShaderStage, fragShaderStage],
          vertexInputState = Just $ SomeStruct vertexInput,
          inputAssemblyState = Just vertexAssemblyInfo,
          tessellationState = Nothing,
          viewportState = Just $ SomeStruct viewportStateInfo,
          rasterizationState = SomeStruct rasterizerInfo,
          multisampleState = Just $ SomeStruct multisampleInfo,
          depthStencilState = Nothing,
          colorBlendState = Just $ SomeStruct colorBlendInfo,
          dynamicState = Nothing,
          layout = pipelineLayout,
          renderPass = renderPass,
          subpass = 0,
          basePipelineHandle = zero,
          basePipelineIndex = zero
        }

      fragShaderStage = SomeStruct $
        PipelineShaderStageCreateInfo
          { next = (),
            flags = zero,
            stage = SHADER_STAGE_FRAGMENT_BIT,
            module' = fragShaderModule,
            name = "main",
            specializationInfo = Nothing
          }
  
      vertexShaderStage = SomeStruct $
        PipelineShaderStageCreateInfo
          { next = (),
            flags = zero,
            stage = SHADER_STAGE_VERTEX_BIT,
            module' = vertShaderModule,
            name = "main",
            specializationInfo = Nothing
          }

      vertexInput = PipelineVertexInputStateCreateInfo
        { next = (),
          flags = zero,
          vertexBindingDescriptions = [vertexBinding],
          vertexAttributeDescriptions = V.fromList vertexAttributes
        }

      vertexAssemblyInfo = PipelineInputAssemblyStateCreateInfo
        { flags = zero,
          topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
          primitiveRestartEnable = False
        }

      viewportStateInfo = PipelineViewportStateCreateInfo
        { next = (),
          flags = zero,
          viewportCount = 1,
          viewports = [viewport],
          scissorCount = 1,
          scissors = [scissor]
        }

      rasterizerInfo = PipelineRasterizationStateCreateInfo
        { next = (),
          flags = zero,
          depthClampEnable = False,
          rasterizerDiscardEnable = False,
          polygonMode = POLYGON_MODE_FILL,
          cullMode = CULL_MODE_BACK_BIT,
          frontFace = FRONT_FACE_CLOCKWISE,
          depthBiasEnable = False,
          depthBiasConstantFactor = 0,
          depthBiasClamp = 0,
          depthBiasSlopeFactor = 0,
          lineWidth = 1
        }

      multisampleInfo = PipelineMultisampleStateCreateInfo
        { next = (),
          flags = zero,
          rasterizationSamples = SAMPLE_COUNT_1_BIT,
          sampleShadingEnable = False,
          minSampleShading = 1,
          sampleMask = [maxBound],
          alphaToCoverageEnable = False,
          alphaToOneEnable = False
        }

      colorBlendInfo = PipelineColorBlendStateCreateInfo
        { next = (),
          flags = zero,
          logicOpEnable = False,
          logicOp = zero,
          attachments = [colorBlendAttachment],
          blendConstants = (0, 0, 0, 0)
        }

      vertexBinding = VertexInputBindingDescription
        { binding = zero,
          stride = fromIntegral $ sizeOf (undefined :: Vertex),
          inputRate = VERTEX_INPUT_RATE_VERTEX
        }

      viewport = Viewport
        { x = 0,
          y = 0,
          width = realToFrac width,
          height = realToFrac height,
          minDepth = 0,
          maxDepth = 1
        }

      scissor = Rect2D
        { offset = Offset2D 0 0,
          extent = extent }

      colorBlendAttachment = PipelineColorBlendAttachmentState
        { blendEnable = False,
          srcColorBlendFactor = BLEND_FACTOR_ONE,
          dstColorBlendFactor = BLEND_FACTOR_ZERO,
          colorBlendOp = BLEND_OP_ADD,
          srcAlphaBlendFactor = BLEND_FACTOR_ONE,
          dstAlphaBlendFactor = BLEND_FACTOR_ZERO,
          alphaBlendOp = BLEND_OP_ADD,
          colorWriteMask = COLOR_COMPONENT_R_BIT
            .|. COLOR_COMPONENT_G_BIT
            .|. COLOR_COMPONENT_B_BIT
            .|. COLOR_COMPONENT_A_BIT
        }
  
  (_, pipelines) <-
    withGraphicsPipelines device pipelineCache [SomeStruct pipelineInfo] Nothing allocate

  return . V.head . snd $ pipelines

withImageViews :: MonadResource m => Device -> SwapchainKHR -> m (V.Vector ImageView)
withImageViews device swapchain = do
  (_, images) <- getSwapchainImagesKHR device swapchain
  for images $ \image ->
    withImageView' device image FORMAT_B8G8R8A8_SRGB

withFramebuffers
 :: MonadResource m
 => Device
 -> V.Vector ImageView
 -> RenderPass
 -> Extent2D
 -> m (V.Vector Framebuffer)
withFramebuffers device imageViews renderPass (Extent2D width height)
  = for imageViews $ \imageView ->
      snd <$> withFramebuffer device (framebufferInfo imageView) Nothing allocate
  where framebufferInfo imageView = FramebufferCreateInfo
          { next = (),
            flags = zero,
            renderPass = renderPass,
            attachments = [imageView],
            width = width,
            height = height,
            layers = 1
          }

withSemaphores :: MonadResource m => Device -> m (Semaphore, Semaphore)
withSemaphores device = unwrapM2 (withSemaphore' zero, withSemaphore' zero)
  where withSemaphore' info = snd <$> withSemaphore device info Nothing allocate

withFence' :: MonadResource m => Device -> m Fence
withFence' device = snd <$> withFence device zero Nothing allocate

withVertexBuffer :: [Vertex] -> Allocator -> Vulkan Buffer
withVertexBuffer vertices allocator = do
  let bufferSize = fromIntegral $ sizeOf (head vertices) * length vertices
      stageFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
      stageUsage = BUFFER_USAGE_TRANSFER_SRC_BIT
      vertUsage = BUFFER_USAGE_TRANSFER_DST_BIT .|. BUFFER_USAGE_VERTEX_BUFFER_BIT
      vertFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  -- Create a staging buffer
  (stagingBuffer, _, stagingAlloc) <- withBuffer' bufferSize stageUsage stageFlags allocator

  -- Upload the vertices
  (unmapStaging, data') <- withMappedMemory allocator stagingAlloc allocate
  liftIO $ pokeArray (castPtr data') vertices
  release unmapStaging

  -- Create the vertex buffer
  (buffer, _, _) <- withBuffer' bufferSize vertUsage vertFlags allocator

  -- Copy the vertices over
  copyBuffer stagingBuffer buffer (fromIntegral bufferSize)
  
  return buffer

withIndexBuffer :: [CUShort] -> Allocator -> Vulkan Buffer
withIndexBuffer indices allocator = do
  let bufferSize = fromIntegral $ sizeOf (head indices) * length indices
      stageFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
      stageUsage = BUFFER_USAGE_TRANSFER_SRC_BIT
      indexFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      indexUsage = BUFFER_USAGE_TRANSFER_DST_BIT .|. BUFFER_USAGE_INDEX_BUFFER_BIT

  -- Create a staging buffer
  (stagingBuffer, _, stagingAlloc) <- withBuffer' bufferSize stageUsage stageFlags allocator

  -- Upload the indices
  (unmapStaging, data') <- withMappedMemory allocator stagingAlloc allocate
  liftIO $ pokeArray (castPtr data') indices
  release unmapStaging

  -- Create the vertex buffer
  (buffer, _, _) <- withBuffer' bufferSize indexUsage indexFlags allocator

  -- Copy the data over
  copyBuffer stagingBuffer buffer (fromIntegral bufferSize)
  
  return buffer

withPipelineLayout'
  :: MonadResource m
  => Device
  -> DescriptorSetLayout
  -> m PipelineLayout
withPipelineLayout' device setLayout = do
  let pipelineLayoutInfo = PipelineLayoutCreateInfo
        { flags = zero,
          setLayouts = [setLayout],
          pushConstantRanges = [] }
  
  (_, pipelineLayout) <- withPipelineLayout device pipelineLayoutInfo Nothing allocate
  return pipelineLayout

withShaderModules :: MonadResource m => Device -> m (ShaderModule, ShaderModule)
withShaderModules device
  = unwrapM2
      ( snd <$> withShaderModule device fragModuleInfo Nothing allocate,
        snd <$> withShaderModule device vertModuleInfo Nothing allocate
      )

  where fragModuleInfo = ShaderModuleCreateInfo
          { next = (),
            flags = zero,
            code = fragShaderCode
          }

        vertModuleInfo = ShaderModuleCreateInfo
          {
            next = (),
            flags = zero,
            code = vertexShaderCode
          }

transitionImageLayout
  :: Image
  -> ImageLayout
  -> ImageLayout
  -> Vulkan ()
transitionImageLayout image oldLayout newLayout = withOneTimeCommands $ do
  (sourceStage, destinationStage, srcAccessMask, dstAccessMask)
    <- case (oldLayout, newLayout) of
         (IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) -> return
           (PIPELINE_STAGE_TOP_OF_PIPE_BIT,
            PIPELINE_STAGE_TRANSFER_BIT,
            zero,
            ACCESS_TRANSFER_WRITE_BIT
           )
         (IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) -> return
           (PIPELINE_STAGE_TRANSFER_BIT,
            PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
            ACCESS_TRANSFER_WRITE_BIT,
            ACCESS_SHADER_READ_BIT
           )
         _ -> liftIO $ throwM $ userError ""
  
  let subRange = ImageSubresourceRange
        { aspectMask = IMAGE_ASPECT_COLOR_BIT,
          baseMipLevel = 0,
          levelCount = 1,
          baseArrayLayer = 0,
          layerCount = 1
        }

      barrier = ImageMemoryBarrier
        { next = (),
          srcAccessMask = srcAccessMask,
          dstAccessMask = dstAccessMask,
          oldLayout = oldLayout,
          newLayout = newLayout,
          srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED,
          dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED,
          image = image,
          subresourceRange = subRange
        }

  commandBuffer <- getCommandBuffer
  cmdPipelineBarrier commandBuffer sourceStage destinationStage zero [] [] [SomeStruct barrier]

withDescriptorSetLayout'
  :: MonadResource m
  => Device
  -> m DescriptorSetLayout
withDescriptorSetLayout' device
  = snd <$> withDescriptorSetLayout device layoutInfo Nothing allocate

  where uniformLayoutBinding = DescriptorSetLayoutBinding
          { binding = 0,
            descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            descriptorCount = 1,
            stageFlags = SHADER_STAGE_VERTEX_BIT,
            immutableSamplers = []
          }

        samplerLayoutBinding = DescriptorSetLayoutBinding
          { binding = 1,
            descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
            descriptorCount = 1,
            stageFlags = SHADER_STAGE_FRAGMENT_BIT,
            immutableSamplers = []
          }

        layoutInfo = DescriptorSetLayoutCreateInfo
          { next = (),
            flags = zero,
            bindings = [uniformLayoutBinding, samplerLayoutBinding]
          }
