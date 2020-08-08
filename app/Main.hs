{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty as Vty
import Lib
import qualified Data.Text.Lazy as TL

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

main :: IO ()
main = render sayHelloTo "Bob"
