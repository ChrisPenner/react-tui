{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Lib where

import Graphics.Vty as Vty
import Control.Monad.Reader
import Control.Monad.State
import Data.TMap as TM
import Data.Typeable
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent
import Control.Monad.STM
import GHC.TypeLits


newtype React a =
    React { runReact :: ReaderT TM.TMap IO a
          }
          deriving newtype (Functor, Applicative, Monad, MonadReader TM.TMap)

newtype Component props =
    Component { runComponent :: props -> React Vty.Image
              }

newtype StateMap = StateMap (TQueue (TM.TMap -> TM.TMap), TVar TM.TMap)

withContext :: Typeable ctx => ctx -> React a  -> React a
withContext ctx = local (TM.insert ctx)

useContext :: Typeable a => React (Maybe a)
useContext = do
    asks TM.lookup

useContextWithDefault :: Typeable a => a -> React a
useContextWithDefault def = fromMaybe def <$> useContext

useState :: forall s. Typeable s => s -> React (s, (s -> s) -> IO ())
useState def = do
    (StateMap (stateQueue, stateMap)) <- fromJust <$> useContext
    s <- React (liftIO (fromMaybe def . TM.lookup <$> readTVarIO stateMap))
    return (s, setState stateQueue)
  where
    setState :: TQueue (TM.TMap -> TM.TMap) -> (s -> s) -> IO ()
    setState var f = atomically $ writeTQueue var $ \typemap ->
        case TM.lookup typemap of
            Nothing -> TM.insert (f def) typemap
            Just a -> TM.insert (f a) typemap

newtype EffectTracker (name :: Symbol) a = EffectTracker (Bool, a)
  deriving Eq

useEffect :: forall name sentinel. (Typeable sentinel, Eq sentinel, KnownSymbol name) => sentinel -> IO () -> React ()
useEffect sentinel effect = do
    (EffectTracker (initialized, sentinel') :: EffectTracker name sentinel, setSentinel) <- useState (EffectTracker @name (False, sentinel))
    if not initialized || sentinel /= sentinel'
       then do
           React (liftIO $ setSentinel (const (EffectTracker @name (True, sentinel'))))
           runEffect
       else return ()
  where
    runEffect = void . React . liftIO $ forkIO effect

newtype EventGetter = EventGetter (IO Vty.Event)


useTermEvent :: (Vty.Event -> IO ()) -> React ()
useTermEvent handler = do
    useContext >>= \case
      Just (EventGetter getEvent) ->
        useEffect @"blah" () $ do
            forever $ getEvent >>= handler
      Nothing -> return ()

newtype Shutdown = Shutdown (IO ())

useShutdown :: React (IO ())
useShutdown = do
    useContext >>= \case
      Just (Shutdown shutdown) ->
          return shutdown
      Nothing -> return (return ())

render ::  Component props -> props -> IO ()
render (Component renderComponent) props = do
    vty <- Vty.mkVty Vty.defaultConfig
    (stateMap :: TVar TM.TMap) <- newTVarIO mempty
    stateQueue <- newTQueueIO
    forever $ do
        pic <- flip runReaderT mempty
             . runReact
             . withContext (StateMap (stateQueue, stateMap))
             .  withContext (EventGetter $ nextEvent vty)
             .  withContext (Shutdown $ shutdown vty)
             $ (renderComponent props)
        update vty (Vty.picForImage pic)
        atomically $ do
            f <- readTQueue stateQueue
            modifyTVar' stateMap f
    getLine
    shutdown vty

renderText :: TL.Text -> React Image
renderText = return . Vty.text Vty.defAttr
