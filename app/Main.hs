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
import React.Editor
import React.App
import React.CoreHooks
import React.Vty
import Data.Text.IO as T
import Data.Text as T
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Control.Concurrent.Async

helloWorld :: Component () Vty.Image
helloWorld = component $ \() -> do
    renderText "Hello, world!"

newtype Name = Name TL.Text

sayHello :: Component () Vty.Image
sayHello = component $ \() -> do
    Name name <- useContextWithDefault (Name "stranger")
    renderText $ "Hello, " <> name

sayHelloTo :: Component TL.Text Vty.Image
sayHelloTo = component $ \name -> do
    withContext (Name name) $ sayHello "hello" ()

simpleStateTracker :: Component () Vty.Image
simpleStateTracker = component (const $ useState (0 :: Int) *> renderText "yo")

timer :: Component () Vty.Image
timer = component $ \() -> do
    (counter, setCounter) <- useState (0 :: Int)
    lstKey <- if counter < 5 || counter > 10 then lastKey "asldkj" (HasKeyboardFocus (counter `mod` 2 == 0), return ())
                    else return mempty
    useAsync () $ forever $ do
                    threadDelay 1000000
                    setCounter succ
    (lstKey Vty.<->) <$> (renderText $ "Counter: " <> TL.pack (show counter))

favNumber :: Component () Vty.Image
favNumber = component $ \() -> do
    (favNumber, _) <- useState (42 :: Int)
    renderText $ "Favourite Number: " <> TL.pack (show favNumber)

newtype HasKeyboardFocus = HasKeyboardFocus Bool
lastKey :: Component (HasKeyboardFocus, IO ()) Vty.Image
lastKey = component $ \(HasKeyboardFocus hasKeyboardFocus, toggle) -> do
    (keypress, setKeypress) <- useState "No events"
    shutdown <- useExit
    debugIO <- useDebugIO
    useTermEvent hasKeyboardFocus $ \evt -> do
      debugIO $ ("hasKeyboard", hasKeyboardFocus)
      case evt of
        Vty.EvKey (Vty.KChar 'q') _ -> shutdown
        Vty.EvKey (Vty.KChar 'c') _ -> shutdown
        Vty.EvKey key _ -> do
            when hasKeyboardFocus $ do
                toggle
                setKeypress (const $ TL.pack $ show key)
        _ -> return ()
    renderText $ "Last Keypress: " <> keypress

boxed :: Component a Vty.Image -> Component a Vty.Image
boxed cmp = component $ \props -> do
    img <- cmp "child" props
    let w = Vty.imageWidth img
    let h = Vty.imageHeight img
    let horBorder = Vty.text defAttr $ TL.replicate (fromIntegral w) "#"
    let vertBorder = vertCat $ L.replicate (h + 2) (Vty.text defAttr "#")
    return $ vertBorder Vty.<|> (horBorder <-> img <-> horBorder) Vty.<|> vertBorder

flipflopper :: Component () Vty.Image
flipflopper = component $ \_ -> do
  (flipflop, setFlipFlop) <- useState False
  useDebug flipflop
  i2 <- lastKey "flip" (HasKeyboardFocus flipflop, setFlipFlop not)
  i3 <- lastKey "flop" (HasKeyboardFocus (not flipflop), setFlipFlop not)
  return ( i2 Vty.<-> i3)

writeLogs :: TQueue T.Text -> String -> String -> IO ()
writeLogs queue compID msg = do
    atomically $ writeTQueue queue $ T.pack (compID <> ": "<> msg <> "\n")

main :: IO ()
main = do
    debugLogsVar <- newTQueueIO
    withAsync (forever $ atomically (readTQueue debugLogsVar) >>= T.appendFile "log") $ const $ do
        runVty (withDebugger (writeLogs debugLogsVar) . fmap snd $ editor "editor" defaultEditorSettings)
