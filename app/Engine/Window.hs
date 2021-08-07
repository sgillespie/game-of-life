module Engine.Window
  ( RefreshLimit(..),
    requiredWindowExts,
    shouldQuit,
    withSdl,
    withWindow,
    withWindowSurface
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text hiding (any)
import Foreign.Ptr
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Control.Monad.Trans.Resource
import SDL
import SDL.Video.Vulkan
import Vulkan.Core10 (Instance(), instanceHandle)
import Vulkan.Extensions.VK_KHR_surface

data RefreshLimit
  = NoLimit
  | TimeLimit Int -- ^ Time in ms
  | EventLimit    -- ^ Indefinite timeout
  deriving (Eq, Show)

requiredWindowExts :: MonadIO m => Window -> m (V.Vector B.ByteString)
requiredWindowExts window
  = liftIO $ vkGetInstanceExtensions window
  >>= traverse B.packCString . V.fromList

withSdl :: MonadResource m => m ()
withSdl = void $ allocate_ (initialize flags) quit
  where flags = [InitEvents] :: [InitFlag]

withWindow :: MonadResource m => Text -> Int -> Int -> m Window
withWindow title width height = do
  initialize ([InitVideo] :: [InitFlag])
  void $ allocate_ (vkLoadLibrary Nothing) vkUnloadLibrary

  let config = defaultWindow
        { windowInitialSize = V2 (fromIntegral width) (fromIntegral height),
          windowGraphicsContext = VulkanContext,
          windowMode = FullscreenDesktop,
          windowResizable = True,
          windowHighDPI = True,
          windowVisible = True
        }

  (_, window) <- allocate (createWindow title config) destroyWindow
  
  pure window

withWindowSurface :: MonadResource m => Instance -> Window -> m (ReleaseKey, SurfaceKHR)
withWindowSurface instance' window = allocate
  (SurfaceKHR <$> vkCreateSurface window (castPtr $ instanceHandle instance'))
  (flip (destroySurfaceKHR instance') Nothing)

shouldQuit :: MonadIO m => RefreshLimit -> m Bool
shouldQuit limit = any isQuitEvent <$> awaitSdlEvents limit

isQuitEvent :: Event -> Bool
isQuitEvent (Event _ QuitEvent) = True
isQuitEvent (Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ code _))))
  | code == SDL.KeycodeQ || code == KeycodeEscape = True
isQuitEvent _ = False

awaitSdlEvents :: MonadIO m => RefreshLimit -> m [Event]
awaitSdlEvents limit = do
  first <- case limit of
    NoLimit -> pure Nothing
    TimeLimit ms -> waitEventTimeout (fromIntegral ms)
    EventLimit -> Just <$> waitEvent

  next <- pollEvents
  pure $ maybeToList first <> next
