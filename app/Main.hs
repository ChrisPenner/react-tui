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
import qualified Data.List as L

import GHC.Clock

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
    withContext (Name name) $ mountComponent sayHello "hello" ()

timer :: Component ()
timer = Component $ \() -> do
    (counter, setCounter) <- useState "counter-state" (0 :: Int)
    useEffect "counter-effect" () $ do
        print "Kicking off timer"
        forever $ do
            threadDelay 1000000
            setCounter succ
    renderText $ "Counter: " <> TL.pack (show counter)

favNumber :: Component ()
favNumber = Component $ \() -> do
    (favNumber, _) <- useState "fav" (42 :: Int)
    renderText $ "Favourite Number: " <> TL.pack (show favNumber)

lastKey :: Component ()
lastKey = cached "last" $ Component $ \() -> do
    debug "Rendered!"

    (keypress, setKeypress) <- useState "last-event" "No events"
    shutdown <- useShutdown
    useTermEvent "listener" $ \case
      Vty.EvKey (Vty.KChar 'q') _ -> shutdown
      Vty.EvKey (Vty.KChar 'c') _ -> shutdown
      Vty.EvKey key _ -> do
          debugIO ("Got a key:", key)
          setKeypress (const $ TL.pack $ show key)
      _ -> return ()
    renderText $ "Last Keypress: " <> keypress

boxed :: Component a -> Component a
boxed cmp = Component $ \props -> do
    img <- mountComponent cmp "child" props
    let w = Vty.imageWidth img
    let h = Vty.imageHeight img
    let horBorder = Vty.text defAttr $ TL.replicate (fromIntegral w) "#"
    let vertBorder = vertCat $ L.replicate (h + 2) (Vty.text defAttr "#")
    return $ vertBorder Vty.<|> (horBorder <-> img <-> horBorder) Vty.<|> vertBorder

something :: Component ()
something = Component $ \_ -> do
  i1 <- mountComponent timer "timer" ()
  i2 <- mountComponent favNumber "fav-number" ()
  i3 <- mountComponent lastKey "last-key" ()
  return (i1 Vty.<-> i2 Vty.<-> i3)

main :: IO ()
main = render something ()
