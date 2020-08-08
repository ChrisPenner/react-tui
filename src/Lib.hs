{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Lib where

import Graphics.Vty as Vty
import Control.Monad.Reader
import Data.TMap as TM
import Data.Typeable
import Data.Maybe
import qualified Data.Text.Lazy as TL


newtype React a =
    React { runReact :: ReaderT TM.TMap IO a
          }
          deriving newtype (Functor, Applicative, Monad, MonadReader TM.TMap)

newtype Component props =
    Component { runComponent :: props -> React Vty.Picture
              }

withContext :: Typeable ctx => ctx -> React a  -> React a
withContext ctx = local (TM.insert ctx)

useContext :: Typeable a => React (Maybe a)
useContext = do
    asks TM.lookup

useContextWithDefault :: Typeable a => a -> React a
useContextWithDefault def = fromMaybe def <$> useContext

render ::  Component props -> props -> IO ()
render (Component renderComponent) props = do
    vty <- Vty.mkVty Vty.defaultConfig
    pic <- flip runReaderT mempty . runReact $ (renderComponent props)
    update vty pic
    getLine
    shutdown vty

renderText :: TL.Text -> React Picture
renderText = return . Vty.picForImage . Vty.text Vty.defAttr
