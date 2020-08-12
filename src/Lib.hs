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
import React
import React.CoreHooks
import React.Component
import React.Vty

-- ✅ Scope component's state
-- ✅ Higher order component: wrapping components
-- Re-render cached on context.
-- Component lifecycle & component cleanup (not sure how this works in Haskell)


newtype RegisterComponent = RegisterComponent (CompID -> React () -> React ())
registerComponent :: CompID -> React () -> React ()
registerComponent compID cleanup = do
    useContext >>= \case
      Nothing -> return ()
      Just (RegisterComponent f) -> f compID cleanup

mountComponent ::  Component props -> String -> props -> React Vty.Image
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
        useMemo () $ registerCleanup $ (useSynchronous . atomically $ updater AwaitChange (const $ ShadowedState mempty))
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

-- useCache :: forall sentinel a. (Typeable a, Eq sentinel, Typeable sentinel) =>  sentinel -> React a -> React a
-- useCache sentinel m = do
--     lastSentinel <- useLast sentinel
--     let propsChanged = lastSentinel /= Just sentinel
--     -- Start off dirty so we render the first time
--     (readIsDirty, setDirty) <- useState' True
--     isDirty <- readIsDirty
--     useSynchronous . atomically $ setDirty False (const False)

--     -- (lastContextDeps, setContextDeps) <- useNonRenderingState  (mempty :: S.Set TypeRep)

--     (val, setVal) <- useNonRenderingState  (Nothing :: Maybe a)
--     case val of
--         Just a | not isDirty && not propsChanged -> return a
--         _ -> do
--             StateMap (getStates, updater) <- fromJust <$> useContextUntraced
--             withContext (StateMap (getStates, \b f -> updater b f *> setDirty True (const True))) $ do
--                 a <- m
--                 setVal (const $ Just a)
--                 return a

newtype ShadowedState = ShadowedState SMap
  deriving newtype (Semigroup, Monoid)
  deriving stock Typeable


renderText :: TL.Text -> React Vty.Image
renderText = return . Vty.text Vty.defAttr
