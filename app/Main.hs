module Main where

import Control.Monad.IO.Class

import Control.Monad.Trans.Resource
import SDL (showWindow, time)

import Engine.Window

main :: IO ()
main = runResourceT $ do
  -- Initialization
  withSdl
  window <- withWindow "Gillespie's Game of Life" 800 600

  -- Go
  start <- (time :: MonadIO m => m Double)

  let frame _ = do
        quit <- shouldQuit (TimeLimit 6)
        if quit
          then pure Nothing
          else pure $ Just ("repeat" :: String)

      loopJust :: MonadIO m => (a -> m (Maybe a)) -> a -> m ()
      loopJust f init' = do
        next <- f init'

        case next of
          Just x -> loopJust f x
          Nothing -> return ()

  showWindow window
  loopJust frame undefined

  return ()
