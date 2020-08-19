{-# LANGUAGE LambdaCase #-}
module React.Vty where

import React
import React.CoreHooks
import React.Component
import Graphics.Vty as Vty
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent.Async
import React.App
import Data.Typeable
import Data.Maybe

newtype EventGetter = EventGetter (TChan Vty.Event)
useTermEvent :: (Typeable sentinel, Eq sentinel) => sentinel -> (Vty.Event -> IO ()) -> React ()
useTermEvent sentinel handler = do
    mEventGetter <- useContext
    debugIO <- useDebugIO
    useAsync sentinel $ do
        debugIO "Kicking off term event"
        case mEventGetter of
            Just (EventGetter eventChan) -> do
                localChan <- atomically $ dupTChan eventChan
                forever $ atomically (readTChan localChan) >>= handler
            Nothing -> return ()

useShutdown :: React (IO ())
useShutdown = do
    useContext >>= \case
      Just (Shutdown shutdown) -> do
        return shutdown
      Nothing -> return (return ())

withEvents :: TChan Vty.Event -> React a -> React a
withEvents chan m = do
    withContext (EventGetter chan) m

newtype Shutdown = Shutdown (IO ())

withShutdown :: Vty.Vty -> React a -> React a
withShutdown vty m = do
    exit <- useExit
    useTermEvent () $ \case
        Vty.EvKey (Vty.KChar 'c') _ -> exit
        _ -> return ()
    withContext (Shutdown (Vty.shutdown vty)) m

useViewport :: React (Int, Int)
useViewport = useContext >>= \case
  Nothing -> return (0, 0)
  Just (Viewport w h) -> return (w, h)

data Viewport = Viewport {viewportWidth :: Int, viewportHeight :: Int}
runVty :: React Vty.Image -> IO ()
runVty m = do
    vty <- Vty.mkVty Vty.defaultConfig
    eventChan <- newBroadcastTChanIO
    (width, height) <- Vty.displayBounds . outputIface $ vty
    withAsync (forever $ Vty.nextEvent vty >>= atomically . writeTChan eventChan) $ const $ do
        run ((withEvents eventChan .  withShutdown vty) $ wrapper width height) $ \img -> do
            Vty.update vty . Vty.picForLayers $ [img]
    Vty.shutdown vty
  where
    wrapper width height = do
        (vp, setViewport) <- useState (Viewport width height)
        useTermEvent () $ \case
            Vty.EvResize w h -> setViewport (const $ Viewport w h)
            _ -> return ()
        withContext vp m
    bgLayer = backgroundFill

