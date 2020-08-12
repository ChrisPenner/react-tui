{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module React.Component where

import React
import Graphics.Vty as Vty
import qualified Data.List as L
import Data.Typeable

newtype CompID = CompID [String]
  deriving newtype (Typeable, Eq, Ord)

instance Show CompID where
  show (CompID path) = L.intercalate "/" $ reverse path

addCompID :: String -> CompID -> CompID
addCompID c (CompID cs) = CompID (c:cs)

newtype Component props =
    Component { renderComponent :: props -> React Vty.Image
              }
