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
import qualified Data.Set as S

-- ✅ Scope component's state
-- ✅ Higher order component: wrapping components
-- Re-render cached on context.
-- Component lifecycle & component cleanup (not sure how this works in Haskell)

type SMap = TRM.TypeRepMap (M.Map (CompID, String))
type EffectName = String
type ComponentID = String

debug :: Show a => a -> React ()
debug = useSynchronous . debugIO
debugIO :: Show a => a -> IO ()
debugIO msg = appendFile "log" (show msg <> "\n")

newtype React a =
    React { runReact :: StateT Int (ReaderT TM.TMap IO) a
          } deriving newtype (Functor, Applicative, Monad, MonadReader TM.TMap)

newtype Component props =
    Component { renderComponent :: props -> React Vty.Image
              }

newtype RegisterComponent = RegisterComponent (CompID -> React () -> React ())
newtype RegisterCleanup = RegisterCleanup (React () -> React ())
mountComponent ::  Component props -> ComponentID -> props -> React Vty.Image
mountComponent (Component {renderComponent}) componentID props = do
    adjustContext (addCompID componentID) $ shadowEffectNames $ shadowStateMap $ renderComponent props
  where
      shadowStateMap :: React a -> React a
      shadowStateMap child = do
        (ShadowedState stateMap, updater) <- useState' (ShadowedState mempty)
        withContext (StateMap (return stateMap, \b f -> coerce updater b f)) $ child
      shadowEffectNames :: React a -> React a
      shadowEffectNames (React m) =
          React . lift $ flip evalStateT 0 m
      trackSubComponents :: React a -> React a
      trackSubComponents m = do
          (compMap, updateCompMap) <- useNonRenderingState (mempty :: M.Map CompID (React ()))
          withContext (RegisterComponent (\compID cleanup -> updateCompMap (M.alter (addCleanup cleanup) compID ))) $ m
      addCleanup :: React () -> Maybe (React ()) -> Maybe (React ())
      addCleanup cleanup Nothing = Just cleanup
      addCleanup cleanup (Just existing) = Just (existing >> cleanup)

cached :: (Typeable props, Eq props) => ComponentID -> Component props -> Component props
cached componentName comp = Component $ \props -> do
    useCache props $ do
        mountComponent comp (componentName <> "cached") props

-- Only for super-quick setup methods, don't expose this.
-- newtype Once a = Once (Maybe a)
-- once :: Typeable a => EffectName -> IO a -> React a
-- once effectName action = do
--     (Once m, setOnce) <- useNonRenderingState effectName (Once Nothing)
--     case m of
--         Just a -> return a
--         Nothing -> do
--             a <- useSynchronous action
--             setOnce . const . Once . Just $ a
--             return a

useCache :: forall sentinel a. (Typeable a, Eq sentinel, Typeable sentinel) =>  sentinel -> React a -> React a
useCache sentinel m = do
    (lastSentinelVar, setSentinel) <- useNonRenderingState sentinel
    let propsChanged = lastSentinelVar /= sentinel
    setSentinel (const sentinel)
    -- Start off dirty so we render the first time
    (isDirty, setDirty) <- useState'  True
    useSynchronous . atomically $ setDirty False (const False)

    -- (lastContextDeps, setContextDeps) <- useNonRenderingState  (mempty :: S.Set TypeRep)

    (val, setVal) <- useNonRenderingState  (Nothing :: Maybe a)
    case val of
        Just a | not isDirty && not propsChanged -> return a
        _ -> do
            StateMap (getStates, updater) <- fromJust <$> useContextUntraced
            withContext (StateMap (getStates, \b f -> updater b f *> setDirty True (const True))) $ do
                a <- m
                setVal (const $ Just a)
                return a

useSynchronous :: IO a -> React a
useSynchronous m = React (liftIO m)

newtype StateMap = StateMap (STM SMap, Bool -> (SMap -> SMap) -> STM ())

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

newtype CompID = CompID [String]
  deriving newtype (Typeable, Eq, Ord, Show)

addCompID :: String -> CompID -> CompID
addCompID c (CompID cs) = CompID (c:cs)

alterTRM :: forall a f. Typeable a => (Maybe (f a) -> Maybe (f a)) -> TRM.TypeRepMap f -> TRM.TypeRepMap f
alterTRM f trm =
    case f (TRM.lookup @a trm) of
        Nothing -> TRM.delete @a trm
        Just r -> TRM.insert @a r trm

getStateToken :: React String
getStateToken = show <$> React (modify succ *> get)

useState' :: forall s. Typeable s => s -> React (s, Bool -> (s -> s) -> STM ())
useState' def = do
    cid <- useContextWithDefault (CompID ["root"])
    stateToken <- (cid,) <$> getStateToken
    (StateMap (getStates, updater)) <- fromJust <$> useContextUntraced
    smap <- React (liftIO (TRM.lookup <$> atomically getStates))
    let s = fromMaybe def $ do
                sm <- smap
                M.lookup stateToken sm
    return (s, \rerender f -> updater rerender $ updateTM stateToken $ f)
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
    (s, setS) <- useState' s
    return (s, atomically . setS True)

useNonRenderingState :: forall s. Typeable s => s -> React (s, (s -> s) -> React ())
useNonRenderingState s = do
    (s, setS) <- useState' s
    return (s, useSynchronous . atomically . setS False)

useMemo :: (Eq a, Typeable a, Typeable b) => a -> React b -> React b
useMemo sentinel action = do
    (sentinel', setSentinel) <- useNonRenderingState Nothing
    (val, setVal) <- useNonRenderingState Nothing
    case val of
        Just output | Just sentinel == sentinel' -> return output
        _ -> do
           setSentinel (const (Just sentinel))
           output <- action
           setVal (const $ Just output)
           return output

-- Effect should return a "cleanup" function to deregister the effect.
useEffect :: forall sentinel a. (Typeable sentinel, Eq sentinel) =>  sentinel -> IO () -> React ()
useEffect sentinel effect = do
    useMemo sentinel $ do runEffect
  where
    runEffect = void . useSynchronous $ forkIO (void effect)

newtype EventGetter = EventGetter (IO Vty.Event)
useTermEvent ::  (Vty.Event -> IO ()) -> React ()
useTermEvent handler = do
    useContext >>= \case
      Just (EventGetter getEvent) ->
        useEffect () $ do
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
             . flip evalStateT 0
             . runReact
             . withContext (StateMap (readTVar stateMapVar, \rerender f -> writeTQueue stateQueue $ (rerender, f)))
             .  withContext (EventGetter $ Vty.nextEvent vty)
             .  withContext (Shutdown . atomically $ writeTVar quitVar True)
             .  withContext (CompID ["root"])
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
