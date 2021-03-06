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

module React.CoreHooks
    ( withContext
    , adjustContext
    , useContext
    , useContextWithDefault
    , useState
    , useOnce
    , useMemo
    , useEffectVar
    , useAsync
    , useAsyncVar
    , useDebug
    , useDebugIO
    , withDebugger
    , useExit
      -- For plugin authors
    , registerCleanup
    , useNonRenderingState
    , useSynchronous
    , getCompID
      -- Internal
    , useNonRenderingStateVar
    , useState'
    , SMap
    , StateMap(..)
    , RegisterCleanup(..)
    , AppAction(..)
    , CompID
    , addCompID
    ) where

import qualified Graphics.Vty as Vty
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.TypeRepMap as TRM
import qualified Data.TMap as TM
import Data.Typeable
import Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent
import Control.Monad.STM
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


import React
import React.Component

useSynchronous :: IO a -> React a
useSynchronous m = React (liftIO m)

useExit :: React (IO ())
useExit = do
    StateMap (_, updater) <- fromJust <$> useContext
    return (atomically $ updater ShutdownApp id)

newtype CompID = CompID [String]
  deriving newtype (Typeable, Eq, Ord)

instance Show CompID where
  show (CompID path) = L.intercalate "/" $ reverse path

addCompID :: String -> CompID -> CompID
addCompID c (CompID cs) = CompID (c:cs)

newtype Debugger = Debugger (CompID -> String -> IO ())
useDebug :: Show a => a -> React ()
useDebug msg = do
    debug <- useDebugIO
    useSynchronous $ debug msg

useDebugIO :: Show a => React (a -> IO ())
useDebugIO = do
    Debugger debug <- useContextWithDefault (Debugger (\_ _ -> return ()))
    compID <- getCompID
    return (debug compID . show)

withDebugger :: (String -> String -> IO ()) -> React a -> React a
withDebugger handler = do
    withContext (Debugger (\compID msg -> handler (show compID) msg))

alterTRM :: forall a f. Typeable a => (Maybe (f a) -> Maybe (f a)) -> TRM.TypeRepMap f -> TRM.TypeRepMap f
alterTRM f trm =
    case f (TRM.lookup @a trm) of
        Nothing -> TRM.delete @a trm
        Just r -> TRM.insert @a r trm

getCompID :: React CompID
getCompID = useContextWithDefault (CompID ["root"])

type SMap = TRM.TypeRepMap (M.Map (CompID, String))
newtype StateMap = StateMap (STM SMap, AppAction -> (SMap -> SMap) -> STM ())

withContext :: Typeable ctx => ctx -> React a -> React a
withContext ctx = local (TM.insert ctx)

adjustContext :: Typeable ctx => (ctx -> ctx) -> React a -> React a
adjustContext f = local (TM.adjust f)

newtype RegisterContextTracking = RegisterContextTracking (TypeRep -> React ())
useContext :: forall a. Typeable a => React (Maybe a)
useContext = do
    -- If we're tracing context usages, add the dependency
    useContextUntraced >>= \case
      Just (RegisterContextTracking register) -> register $ typeRep (Proxy @a)
      _ -> return ()
    useContextUntraced

useContextUntraced :: Typeable a => React (Maybe a)
useContextUntraced = do
    asks TM.lookup

useContextWithDefault :: Typeable a => a -> React a
useContextWithDefault def = fromMaybe def <$> useContext

useState' :: forall s. Typeable s => s -> React (React s, AppAction -> (s -> s) -> STM ())
useState' def = do
    cid <- getCompID
    stateToken <- (cid,) <$> getStateToken
    (StateMap (getStates, updater)) <- fromJust <$> useContextUntraced
    let lookupVal = do
            smap <- React (liftIO (TRM.lookup <$> atomically getStates))
            return $ fromMaybe def $ do
                        sm <- smap
                        M.lookup stateToken sm
    return (lookupVal, \rerender f -> updater rerender $ updateTM stateToken $ f)
  where
    updateTM :: (CompID, String) -> (s -> s) -> (SMap -> SMap)
    updateTM token f typemap = do
        flip alterTRM typemap $ \case
          Nothing -> Just $ (M.singleton token $ f def)
          Just r -> Just $ flip (flip M.alter token) r $ \case
            Nothing -> Just $ f def
            Just a -> Just $ f a

useState :: forall s. Typeable s => s -> React (s, (s -> s) -> IO ())
useState s = do
    (ms, setS) <- useState' s
    s <- ms
    return (s, atomically . setS ReRender)

useNonRenderingState :: forall s. Typeable s => s -> React (s, (s -> s) -> React ())
useNonRenderingState s = do
    (readS, setS) <- useNonRenderingStateVar s
    (, setS) <$> readS

useNonRenderingStateVar :: forall s. Typeable s => s -> React (React s, (s -> s) -> React ())
useNonRenderingStateVar s = do
    (s, setS) <- useState' s
    return (s, useSynchronous . atomically . setS AwaitChange)

useMemo :: (Eq a, Typeable a, Typeable b) => a -> React b -> React b
useMemo sentinel action = do
    sentinel' <- useLast sentinel
    (val, setVal) <- useNonRenderingState Nothing
    case val of
        Just output | Just sentinel == sentinel' -> return output
        _ -> do
           output <- action
           setVal (const $ Just output)
           return output

-- Effect should return a "cleanup" function to deregister the effect.
useEffect :: forall sentinel a. (Typeable sentinel, Eq sentinel) =>  sentinel -> IO (IO ()) -> React ()
useEffect sentinel effect = do
    (prevCleanup, setCleanup) <- useNonRenderingState @(IO ()) (return ())
    lastSentinel <- useLast sentinel
    case lastSentinel of
        Just s | s == sentinel -> return ()
        _ -> do
            useSynchronous . async $ prevCleanup
            cleanup <- useSynchronous $ effect
            registerCleanup cleanup
            setCleanup (const cleanup)

-- Effect should return a "cleanup" function to deregister the effect.
useEffectVar :: forall sentinel. (Typeable sentinel) =>  sentinel -> (TVar sentinel -> IO (IO ())) -> React ()
useEffectVar sentinel effect = do
    tVar <- useMemo () $ useSynchronous $ newTVarIO sentinel
    useSynchronous . atomically $ writeTVar tVar sentinel
    cleanup <- useSynchronous $ effect tVar
    registerCleanup cleanup

-- Effect should return a "cleanup" function to deregister the effect.
useAsyncVar :: forall sentinel. (Typeable sentinel) => sentinel -> (TVar sentinel -> IO ()) -> React ()
useAsyncVar sentinel effect = do
    useEffectVar sentinel $ \tvar -> do
        handle <- async (effect tvar)
        return (cancel handle)

-- A wrapper around use-effect which runs the effect asyncronously and calls cancel on it for
-- cleanup
useAsync :: (Typeable sentinel, Eq sentinel) => sentinel -> IO () -> React ()
useAsync s m = useEffect s $ do
    handle <- async m
    return (cancel handle)

getStateToken :: React String
getStateToken = show <$> React (modify succ *> get)

useOnce :: Typeable a => React a -> React a
useOnce = useMemo ()

useUnmount :: IO () -> React ()
useUnmount m = do
    useMemo () $ registerCleanup m

newtype RegisterCleanup = RegisterCleanup (IO () -> React ())
registerCleanup :: IO () -> React ()
registerCleanup m = do
    useContext >>= \case
      Nothing -> return ()
      Just (RegisterCleanup f) -> f m


-- Get the value of the provided value from the previous render if available
useLast :: Typeable a => a -> React (Maybe a)
useLast a = do
    (val, setVal) <- useNonRenderingState Nothing
    setVal (const (Just a))
    return val

data AppAction = ReRender | AwaitChange | ShutdownApp
  deriving Eq
instance Semigroup AppAction where
  ShutdownApp <> _ = ShutdownApp
  _ <> ShutdownApp = ShutdownApp
  ReRender <> _ = ReRender
  _ <> ReRender = ReRender
  AwaitChange <> AwaitChange = AwaitChange

instance Monoid AppAction where
  mempty = AwaitChange
