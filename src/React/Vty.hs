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

newtype EventGetter = EventGetter (TChan Vty.Event)
useTermEvent :: (Typeable sentinel, Eq sentinel) => sentinel -> (Vty.Event -> IO ()) -> React ()
useTermEvent sentinel handler = do
    mEventGetter <- useContext
    debugIO <- useDebugIO 
    useEffect sentinel $ do
        debugIO "Kicking off term event"
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

withEvents :: TChan Vty.Event -> React a -> React a
withEvents chan m = do
    withContext (EventGetter chan) m

newtype Shutdown = Shutdown (IO ())

withShutdown :: Vty.Vty -> React a -> React a
withShutdown vty m = do
    withContext (Shutdown (Vty.shutdown vty)) m

runVty :: React Vty.Image -> IO ()
runVty r = do
    vty <- Vty.mkVty Vty.defaultConfig
    eventChan <- newBroadcastTChanIO
    withAsync (forever $ Vty.nextEvent vty >>= atomically . writeTChan eventChan) $ const $ do
        run ((withEvents eventChan .  withShutdown vty) r) (Vty.update vty . Vty.picForImage)
    Vty.shutdown vty
