{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Graphics.Vty as Vty
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

main :: IO ()
main = render timer ()
