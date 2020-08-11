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
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Concurrent.Async
import qualified Data.List as L

-- ✅ Scope component's state
-- ✅ Higher order component: wrapping components
-- Re-render cached on context.
-- Component lifecycle & component cleanup (not sure how this works in Haskell)

type SMap = TRM.TypeRepMap (M.Map (CompID, String))
type EffectName = String
type ComponentID = String

debug :: Show a => a -> React ()
debug msg = do
    compID <- getCompID
    useSynchronous $ debugIO compID msg
debugIO :: Show a => CompID -> a -> IO ()
debugIO compID msg = appendFile "log" (show compID <> ": " <> show msg <> "\n")

newtype React a =
    React { runReact :: StateT Int (ReaderT TM.TMap IO) a
          } deriving newtype (Functor, Applicative, Monad, MonadReader TM.TMap)
            deriving (Semigroup, Monoid) via (Ap React a)

newtype Component props =
    Component { renderComponent :: props -> React Vty.Image
              }

newtype RegisterComponent = RegisterComponent (CompID -> React () -> React ())
registerComponent :: CompID -> React () -> React ()
registerComponent compID cleanup = do
    useContext >>= \case
      Nothing -> return ()
      Just (RegisterComponent f) -> f compID cleanup

newtype RegisterCleanup = RegisterCleanup (React () -> React ())
registerCleanup :: React () -> React ()
registerCleanup m = do
    useContext >>= \case
      Nothing -> return ()
      Just (RegisterCleanup f) -> f m

mountComponent ::  Component props -> ComponentID -> props -> React Vty.Image
mountComponent (Component {renderComponent}) componentID props = do
    parentCompID <- getCompID
    (a, cleanup) <- adjustContext (addCompID componentID) $ shadowEffectNames $ trackSubComponents $ trackCleanup $ shadowStateMap $ renderComponent props
    registerComponent (addCompID componentID parentCompID) cleanup
    return a
  where
      shadowStateMap :: React a -> React a
      shadowStateMap child = do
        (readStateMap, updater) <- useState' (ShadowedState mempty)
        -- Clear component state on unmount
        useMemo () $ registerCleanup $ (useSynchronous . atomically $ updater False (const $ ShadowedState mempty))
        ShadowedState stateMap <- readStateMap
        withContext (StateMap (return stateMap, \b f -> coerce updater b f)) $ child
      shadowEffectNames :: React a -> React a
      shadowEffectNames (React m) =
          React . lift $ flip evalStateT 0 m
      trackCleanup :: React a -> React (a, React ())
      trackCleanup action = do
          (readCleanup, setCleanup) <- useNonRenderingStateVar mempty
          withContext (RegisterCleanup (\m -> setCleanup (>> m))) $ do
              a <- action
              (a,) <$> readCleanup

      -- State inside this function just doesn't work and I have no clue why.
      trackSubComponents :: React a -> React a
      trackSubComponents m = do
          compMapVar <- useMemo () . useSynchronous $ newTVarIO (mempty :: M.Map CompID (React ()))
          prevCompMap <- useSynchronous $ readTVarIO compMapVar
          useSynchronous . atomically . writeTVar compMapVar $ mempty
          a <- withContext (RegisterComponent (\compID cleanup -> do
              useSynchronous . atomically $ modifyTVar' compMapVar (M.alter (addCleanup cleanup) compID))) $ m
          newCompMap <- useSynchronous . readTVarIO $ compMapVar
          let unmountedComponents = M.difference prevCompMap newCompMap
          -- Run any relevant cleanup
          fold $ unmountedComponents
          return a
      addCleanup :: React () -> Maybe (React ()) -> Maybe (React ())
      addCleanup cleanup Nothing = Just cleanup
      addCleanup cleanup (Just existing) = Just (existing >> cleanup)

-- This is broken right now
-- cached :: (Typeable props, Eq props) => ComponentID -> Component props -> Component props
-- cached componentName comp = Component $ \props -> do
--     useCache props $ do
--         mountComponent comp (componentName <> "cached") props

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
    lastSentinel <- useLast sentinel
    let propsChanged = lastSentinel /= Just sentinel
    -- Start off dirty so we render the first time
    (readIsDirty, setDirty) <- useState' True
    isDirty <- readIsDirty
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

-- Get the value of the provided value from the previous render if available
useLast :: Typeable a => a -> React (Maybe a)
useLast a = do
    (val, setVal) <- useNonRenderingState Nothing
    setVal (const (Just a))
    return val

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
  deriving newtype (Typeable, Eq, Ord)

instance Show CompID where
  show (CompID path) = L.intercalate "/" $ reverse path

addCompID :: String -> CompID -> CompID
addCompID c (CompID cs) = CompID (c:cs)

alterTRM :: forall a f. Typeable a => (Maybe (f a) -> Maybe (f a)) -> TRM.TypeRepMap f -> TRM.TypeRepMap f
alterTRM f trm =
    case f (TRM.lookup @a trm) of
        Nothing -> TRM.delete @a trm
        Just r -> TRM.insert @a r trm

getStateToken :: React String
getStateToken = show <$> React (modify succ *> get)

getCompID :: React CompID
getCompID = useContextWithDefault (CompID ["root"])

useState' :: forall s. Typeable s => s -> React (React s, Bool -> (s -> s) -> STM ())
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
    return (s, atomically . setS True)

useNonRenderingState :: forall s. Typeable s => s -> React (s, (s -> s) -> React ())
useNonRenderingState s = do
    (readS, setS) <- useNonRenderingStateVar s
    (, setS) <$> readS

useNonRenderingStateVar :: forall s. Typeable s => s -> React (React s, (s -> s) -> React ())
useNonRenderingStateVar s = do
    (s, setS) <- useState' s
    return (s, useSynchronous . atomically . setS False)

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
useEffect :: forall sentinel a. (Typeable sentinel, Eq sentinel) =>  sentinel -> IO () -> React ()
useEffect sentinel effect = do
    useMemo sentinel $ do runEffect
  where
    runEffect = do
        debug "Registering Cancel"
        handle <- useSynchronous $ async (void effect)
        registerCleanup (debug "Cancelling! ">> useSynchronous (cancel handle) >> debug "Thread killed")

newtype EventGetter = EventGetter (IO Vty.Event)
useTermEvent ::  (Vty.Event -> IO ()) -> React ()
useTermEvent handler = do
    mEventGetter <- useContext
    useEffect () $ do
        case mEventGetter of
            Just (EventGetter getEvent) -> forever $ getEvent >>= handler >> debugIO (CompID ["Events"]) "Tick"
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
render comp props = do
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
             $ (mountComponent comp "root" props)
        Vty.update vty (Vty.picForImage pic)
        appState <- fix $ \recurse -> do
            appState <- atomically $ do
                let checkQuit = readTVar quitVar >>= check >> return ShutdownApp
                orElse checkQuit $ do
                        updates <- flushTQueue stateQueue
                        when (null updates) retry
                        rerenders <- for updates $ \(rerender, f) -> do
                                            modifyTVar' stateMapVar f
                                            return rerender
                        if or rerenders then return ReRender
                                        else return AwaitChange
            if appState == AwaitChange
               then recurse
               else return appState
        case appState of
            ShutdownApp -> Vty.shutdown vty
            _ -> loop

renderText :: TL.Text -> React Vty.Image
renderText = return . Vty.text Vty.defAttr
