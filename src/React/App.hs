{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingVia #-}
module React.App where

import qualified Graphics.Vty as Vty
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.TypeRepMap as TRM
import qualified Data.TMap as TM
import Data.Typeable
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent
import Control.Monad.STM
import GHC.TypeLits
import Data.Coerce
import qualified Data.Map as M
import Data.Bifunctor
import qualified Data.Set as S
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Concurrent.Async
import qualified Data.List as L
import React
import React.CoreHooks
import React.Component

run ::  React a -> (a -> IO ()) -> IO a
run react handler = do
    (stateMapVar :: TVar SMap) <- newTVarIO mempty
    (stateQueue :: TQueue ((AppAction, SMap -> SMap))) <- newTQueueIO
    quitVar <- newTVarIO False
    fix $ \loop -> do
        renderResult <- flip runReaderT mempty
             . flip evalStateT 0
             . runReact
             . withContext (StateMap (readTVar stateMapVar, \rerender f -> writeTQueue stateQueue $ (rerender, f)))
             $ react
        handler renderResult
        appState <- fix $ \recurse -> do
            appState <- atomically $ do
                let checkQuit = readTVar quitVar >>= check >> return ShutdownApp
                orElse checkQuit $ do
                        updates <- flushTQueue stateQueue
                        when (null updates) retry
                        fmap fold . for updates $ \(rerender, f) -> do
                                            modifyTVar' stateMapVar f
                                            return rerender
            if appState == AwaitChange
               then recurse
               else return appState
        case appState of
            ShutdownApp -> return renderResult
            _ -> loop
