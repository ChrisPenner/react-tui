{-# LANGUAGE LambdaCase #-}
module React.Vty where

import React
import React.CoreHooks
import React.Component
import Graphics.Vty as Vty
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad
import React.App

newtype EventGetter = EventGetter (TChan Vty.Event)
useTermEvent ::  (Vty.Event -> IO ()) -> React ()
useTermEvent handler = do
    mEventGetter <- useContext
    useEffect () $ do
        case mEventGetter of
            Just (EventGetter eventChan) -> do
                localChan <- atomically $ dupTChan eventChan
                forever $ atomically (readTChan localChan) >>= handler
            Nothing -> return ()

useShutdown :: React (IO ())
useShutdown = do
    useContext >>= \case
      Just (Shutdown shutdown) ->
          return shutdown
      Nothing -> return (return ())

withEvents :: Vty.Vty -> React a -> React a
withEvents vty m = do
    eventChan <- useOnce . useSynchronous $ newBroadcastTChanIO
    withContext (EventGetter eventChan) m

newtype Shutdown = Shutdown (IO ())

withShutdown :: Vty.Vty -> React a -> React a
withShutdown vty m = do
    withContext (Shutdown (Vty.shutdown vty)) m

runVty :: React Vty.Image -> IO ()
runVty r = do
    vty <- Vty.mkVty Vty.defaultConfig
    run ((withEvents vty .  withShutdown vty) r) (Vty.update vty . Vty.picForImage)
    Vty.shutdown vty
