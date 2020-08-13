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
import Control.Monad.State
import React.Component
import React
import React.App
import React.CoreHooks
import React.Vty

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

simpleStateTracker :: Component ()
simpleStateTracker = Component (const $ useState (0 :: Int) *> renderText "yo")

timer :: Component ()
timer = Component $ \() -> do
    -- debug "rendered timer"
    (counter, setCounter) <- useState (0 :: Int)
    -- lstKey <- mountComponent lastKey "asldkj" ()
    lstKey <- if counter < 5 || counter > 10 then mountComponent lastKey "asldkj" ()
                    else return mempty
    useEffect () $ do
        forever $ do
            threadDelay 1000000
            setCounter succ
    (lstKey Vty.<->) <$> (renderText $ "Counter: " <> TL.pack (show counter))

favNumber :: Component ()
favNumber = Component $ \() -> do
    (favNumber, _) <- useState (42 :: Int)
    renderText $ "Favourite Number: " <> TL.pack (show favNumber)

lastKey :: Component ()
lastKey = Component $ \() -> do
    -- debug "rendered lastKey"
    (keypress, setKeypress) <- useState "No events"
    shutdown <- useExit
    useTermEvent $ \case
      Vty.EvKey (Vty.KChar 'q') _ -> shutdown
      Vty.EvKey (Vty.KChar 'c') _ -> shutdown
      Vty.EvKey key _ -> do
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
  -- i2 <- mountComponent favNumber "fav-number" ()
  i2 <- mountComponent lastKey "last-key" ()
  i3 <- mountComponent lastKey "laster-key" ()
  return (i1 Vty.<-> i2 Vty.<-> i3)

-- data Selected = A | B | C
--   deriving (Eq, Ord)
-- routeFocus :: Component ()
-- routeFocus = Component $ \_ -> do
--     (selected, setSelected) <- useState A
--     case selected of

main :: IO ()
main = runVty (mountComponent something "something" ())
