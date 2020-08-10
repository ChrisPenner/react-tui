{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
module Lib where

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

-- ✅ Scope component's state
-- ✅ Higher order component: wrapping components
-- Component lifecycle

type SMap = TRM.TypeRepMap (M.Map (String, String))
type EffectName = String
type ComponentID = String

debug :: Show a => a -> React ()
debug = useSynchronous . debugIO
debugIO :: Show a => a -> IO ()
debugIO msg = appendFile "log" (show msg <> "\n")

newtype React a =
    React { runReact :: ReaderT TM.TMap IO a
          }
          deriving newtype (Functor, Applicative, Monad, MonadReader TM.TMap)

newtype Component props =
    Component { renderComponent :: props -> React Vty.Image
              }

mountComponent ::  Component props -> ComponentID -> props -> React Vty.Image
mountComponent (Component {renderComponent}) componentID props = do
    shadowStateMap componentID (renderComponent props)

cached :: (Typeable props, Eq props) => ComponentID -> Component props -> Component props
cached componentName comp = Component $ \props -> do
    useCache componentName props $ do
        mountComponent comp (componentName <> "cached") props

newtype Once a = Once (Maybe a)
-- Only for super-quick setup methods, don't expose this.
once :: Typeable a => EffectName -> IO a -> React a
once effectName action = do
    (Once m, setOnce) <- useNonRenderingState effectName (Once Nothing)
    case m of
        Just a -> return a
        Nothing -> do
            a <- useSynchronous action
            setOnce . const . Once . Just $ a
            return a

useCache :: forall sentinel a. (Typeable a, Eq sentinel, Typeable sentinel) => EffectName -> sentinel -> React a -> React a
useCache effectName sentinel m = do
    StateMap (getStates, updater) <- fromJust <$> useContext
    (lastSentinelVar, setSentinel) <- useNonRenderingState (effectName <> "sentinel") sentinel
    let propsChanged = lastSentinelVar /= sentinel
    setSentinel (const sentinel)
    -- Start off dirty so we render the first time
    (isDirty, setDirty) <- useState' (effectName <> "dirty") True
    useSynchronous . atomically $ setDirty False (const False)
    (val, setVal) <- useNonRenderingState (effectName <> "cacheState") (Nothing :: Maybe a)
    case val of
        Just a | not isDirty && not propsChanged -> return a
        _ -> do
            withContext (StateMap (getStates, \b f -> updater b f *> setDirty True (const True))) $ do
                a <- m
                setVal (const $ Just a)
                return a

useSynchronous :: IO a -> React a
useSynchronous m = React (liftIO m)

newtype StateMap = StateMap (STM SMap, Bool -> (SMap -> SMap) -> STM ())

withContext :: Typeable ctx => ctx -> React a -> React a
withContext ctx = local (TM.insert ctx)

useContext :: Typeable a => React (Maybe a)
useContext = do
    asks TM.lookup

useContextWithDefault :: Typeable a => a -> React a
useContextWithDefault def = fromMaybe def <$> useContext

newtype CompID = CompID String
  deriving newtype Typeable

alterTRM :: forall a f. Typeable a => (Maybe (f a) -> Maybe (f a)) -> TRM.TypeRepMap f -> TRM.TypeRepMap f
alterTRM f trm =
    case f (TRM.lookup @a trm) of
        Nothing -> TRM.delete @a trm
        Just r -> TRM.insert @a r trm

useState' :: forall s. Typeable s => String -> s -> React (s, Bool -> (s -> s) -> STM ())
useState' stateID def = do
    CompID cid <- useContextWithDefault (CompID "")
    (StateMap (getStates, updater)) <- fromJust <$> useContext
    smap <- React (liftIO (TRM.lookup <$> atomically getStates))
    let s = fromMaybe def $ do
                sm <- smap
                M.lookup (cid, stateID) sm
    return (s, \rerender f -> updater rerender $ updateTM cid $ f)
  where
    updateTM :: String -> (s -> s) -> (SMap -> SMap)
    updateTM cid f typemap = do
        flip alterTRM typemap $ \case
          Nothing -> Just $ (M.singleton (cid, stateID) $ f def)
          Just r -> Just $ flip (flip M.alter (cid, stateID)) r $ \case
            Nothing -> Just $ f def
            Just a -> Just $ f a

useState :: forall s. Typeable s => String -> s -> React (s, (s -> s) -> IO ())
useState stateID s = (fmap . fmap) (fmap atomically . ($ True)) $ useState' stateID s
useNonRenderingState :: forall s. Typeable s => String -> s -> React (s, (s -> s) -> React ())
useNonRenderingState stateID s = (fmap . fmap) (\f -> useSynchronous . atomically . f False) $ useState' stateID s

newtype EffectTracker a = EffectTracker (Bool, a)
  deriving Eq

useEffect :: forall sentinel. (Typeable sentinel, Eq sentinel) => EffectName -> sentinel -> IO () -> React ()
useEffect effectName sentinel effect = do
    (EffectTracker (initialized, sentinel') :: EffectTracker sentinel, setSentinel) <- useNonRenderingState effectName (EffectTracker (False, sentinel))
    if not initialized || sentinel /= sentinel'
       then do
           setSentinel (const (EffectTracker (True, sentinel')))
           runEffect
       else return ()
  where
    runEffect = void . React . liftIO $ forkIO effect

newtype EventGetter = EventGetter (IO Vty.Event)


useTermEvent :: EffectName -> (Vty.Event -> IO ()) -> React ()
useTermEvent effectName handler = do
    useContext >>= \case
      Just (EventGetter getEvent) ->
        useEffect effectName () $ do
            forever $ getEvent >>= handler
      Nothing -> return ()

newtype Shutdown = Shutdown (IO ())

useShutdown :: React (IO ())
useShutdown = do
    useContext >>= \case
      Just (Shutdown shutdown) ->
          return shutdown
      Nothing -> return (return ())

newtype ShadowedState = ShadowedState SMap
  deriving newtype (Semigroup, Monoid)
  deriving stock Typeable

shadowStateMap :: EffectName -> React a -> React a
shadowStateMap effectName child = do
    (ShadowedState stateMap, updater) <- useState' effectName (ShadowedState mempty)
    withContext (StateMap (return stateMap, \b f -> coerce updater b f)) child

data AppState = ReRender | AwaitChange | ShutdownApp
  deriving Eq
render ::  Component props -> props -> IO ()
render (Component renderComponent) props = do
    vty <- Vty.mkVty Vty.defaultConfig
    (stateMapVar :: TVar SMap) <- newTVarIO mempty
    stateQueue <- newTQueueIO
    quitVar <- newTVarIO False
    fix $ \loop -> do
        pic <- flip runReaderT mempty
             . runReact
             . withContext (StateMap (readTVar stateMapVar, \rerender f -> writeTQueue stateQueue $ (rerender, f)))
             .  withContext (EventGetter $ Vty.nextEvent vty)
             .  withContext (Shutdown . atomically $ writeTVar quitVar True)
             .  withContext (CompID "root")
             $ (renderComponent props)
        Vty.update vty (Vty.picForImage pic)
        appState <- fix $ \recurse -> do
            appState <- atomically $ do
                let checkQuit = readTVar quitVar >>= check >> return ShutdownApp
                orElse checkQuit $ do
                    (rerender, f) <- readTQueue stateQueue
                    modifyTVar' stateMapVar f
                    if rerender then return ReRender
                                else return AwaitChange
            if appState == AwaitChange
               then recurse
               else return appState
        case appState of
            ShutdownApp -> Vty.shutdown vty
            _ -> loop

renderText :: TL.Text -> React Vty.Image
renderText = return . Vty.text Vty.defAttr
