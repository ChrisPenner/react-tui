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

import Graphics.Vty as Vty
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

-- ✅ Scope component's state
-- ✅ Higher order component: wrapping components
-- Component lifecycle

type SMap = TRM.TypeRepMap (M.Map (String, String))
type EffectName = String
type ComponentID = String

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

cached :: (Typeable props, Eq props) => Component props -> Component props
cached comp = Component $ \props -> do
    useCache props $ do
        mountComponent comp "child2" props

useCache :: forall sentinel a. (Typeable a, Eq sentinel, Typeable sentinel) => sentinel -> React a -> React a
useCache sentinel m = shadowStateMap "cached" $ do
    (val, setVal) <- useNonRenderingState "val-cache" (Nothing :: Maybe a)
    (savedSentinel, setSentinel) <- useNonRenderingState "sentinel" sentinel
    let refreshCache = do
            newVal <- m
            useSynchronous (setVal (const (Just newVal)))
            return newVal
    result <- case (val, savedSentinel == sentinel) of
        (Nothing, _) -> refreshCache
        (_, False) -> refreshCache
        (Just valCache, _) -> return valCache
    useSynchronous $ (setSentinel (const sentinel))
    return result

useSynchronous :: IO a -> React a
useSynchronous m = React (liftIO m)

newtype StateMap = StateMap (IO SMap, Bool -> (SMap -> SMap) -> IO ())

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

useState' :: forall s. Typeable s => String -> s -> React (s, Bool -> (s -> s) -> IO ())
useState' stateID def = do
    CompID cid <- useContextWithDefault (CompID "")
    (StateMap (getStates, updater)) <- fromJust <$> useContext
    smap <- React (liftIO (TRM.lookup <$> getStates))
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
useState stateID s = (fmap . fmap) ($ True) $ useState' stateID s
useNonRenderingState :: forall s. Typeable s => String -> s -> React (s, (s -> s) -> IO ())
useNonRenderingState stateID s = (fmap . fmap) ($ False) $ useState' stateID s

newtype EffectTracker a = EffectTracker (Bool, a)
  deriving Eq

useEffect :: forall sentinel. (Typeable sentinel, Eq sentinel) => EffectName -> sentinel -> IO () -> React ()
useEffect effectName sentinel effect = do
    (EffectTracker (initialized, sentinel') :: EffectTracker sentinel, setSentinel) <- useState effectName (EffectTracker (False, sentinel))
    if not initialized || sentinel /= sentinel'
       then do
           React (liftIO $ setSentinel (const (EffectTracker (True, sentinel'))))
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
    withContext (StateMap (return stateMap, coerce updater)) child

render ::  Component props -> props -> IO ()
render (Component renderComponent) props = do
    vty <- Vty.mkVty Vty.defaultConfig
    (stateMapVar :: TVar SMap) <- newTVarIO mempty
    stateQueue <- newTQueueIO
    forever $ do
        pic <- flip runReaderT mempty
             . runReact
             . withContext (StateMap (readTVarIO stateMapVar, \rerender f -> atomically . writeTQueue stateQueue $ (rerender, f)))
             .  withContext (EventGetter $ nextEvent vty)
             .  withContext (Shutdown $ shutdown vty)
             .  withContext (CompID "root")
             $ (renderComponent props)
        update vty (Vty.picForImage pic)
        fix $ \recurse -> do
            rerender <- atomically $ do
                (rerender, f) <- readTQueue stateQueue
                modifyTVar' stateMapVar f
                return rerender
            if rerender
                then return ()
                else recurse
    shutdown vty

renderText :: TL.Text -> React Image
renderText = return . Vty.text Vty.defAttr
