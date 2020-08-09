{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Graphics.Vty as Vty
import Graphics.Vty.Input.Events as Vty
import Lib
import qualified Data.Text.Lazy as TL
import Control.Concurrent
import Control.Monad


helloWorld :: Component ()
helloWorld = Component $ \_ -> do
    renderText "Hello, world!"

newtype Name = Name TL.Text

sayHello :: Component ()
sayHello = Component $ \_ -> do
    Name name <- useContextWithDefault (Name "stranger")
    renderText $ "Hello, " <> name

sayHelloTo :: Component TL.Text
sayHelloTo = Component $ \name -> do
    withContext (Name name) $ runComponent sayHello ()


timer :: Component ()
timer = Component $ \() -> do
    (counter, setCounter) <- useState (0 :: Int)
    useEffect @"counter" () $ do
        forever $ do
            threadDelay 1000000
            setCounter succ
    renderText $ "Counter: " <> TL.pack (show counter)

lastKey :: Component ()
lastKey = Component $ \() -> do
    (keypress, setKeypress) <- useState "No events"
    shutdown <- useShutdown
    useTermEvent $ \case
      Vty.EvKey (Vty.KChar 'q') _ -> shutdown
      Vty.EvKey key _ ->
          setKeypress (const $ TL.pack $ show key)
      _ -> return ()
    renderText $ "Last Keypress: " <> keypress

something :: Component ()
something = Component $ \_ -> do
  i1 <- runComponent timer ()
  i2 <- runComponent lastKey ()
  return (i1 Vty.<-> i2)

main :: IO ()
main = render something ()
