{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module React where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.TMap as TM
import Data.Monoid

newtype React a =
    React { runReact :: StateT Int (ReaderT TM.TMap IO) a
          } deriving newtype (Functor, Applicative, Monad, MonadReader TM.TMap)
            deriving (Semigroup, Monoid) via (Ap React a)

