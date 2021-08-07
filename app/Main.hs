module Main where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Vector (Vector())

import Control.Monad.Trans.Resource
import SDL (Window(), showWindow, time)
import Vulkan.Core10

import Engine.Domain
import Engine.GraphicsPipeline
import Engine.Init
import Engine.Monad
import Engine.Render
import Engine.Window

main :: IO ()
main = runResourceT $ do
  -- Initialization
  withSdl
  window <- withWindow "Gillespie's Game of Life" 800 600
  windowExts <- requiredWindowExts window
  instance' <- withInstance' windowExts
  (_, surface) <- withWindowSurface instance' window

  handles <- withVulkanHandles instance' surface

  -- Go
  start <- (time :: MonadIO m => m Double)

  let frame :: Window -> Vector CommandBuffer -> Frame -> Vulkan (Maybe Frame)
      frame window buffers frame' = do
        quit <- shouldQuit (TimeLimit 6)
        if quit
          then pure Nothing
          else do
            runFrame frame' (renderFrame buffers)
            Just <$> advanceFrame frame'

      loopJust :: MonadIO m => (a -> m (Maybe a)) -> a -> m ()
      loopJust f init' = f init' >>= mapM_ (loopJust f)

  runVulkan handles $ do
    reportPhysicalDevice
    showWindow window

    initialFrame <- withVulkanFrame surface
    commandBuffers <- withCommandBuffers' initialFrame
    runCommandBuffers initialFrame commandBuffers
    
    loopJust (frame window commandBuffers) initialFrame

reportPhysicalDevice :: Vulkan ()
reportPhysicalDevice = do
  physicalDevice <- getPhysicalDevice
  props <- getPhysicalDeviceProperties physicalDevice

  let PhysicalDeviceProperties{deviceType=type', deviceName=name'} = props

  liftIO . putStrLn $ "Using device: " ++ show name' ++ " (" ++ show type' ++ ")"
