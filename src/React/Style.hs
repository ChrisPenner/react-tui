{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module React.Style where

import React
import React.CoreHooks
import React.Vty
import qualified Graphics.Vty as Vty
import Data.Traversable
import Data.Monoid
import Control.Lens
import Lib

newtype Width a = Width {getWidth :: a}
newtype Height a = Height {getHeight :: a}

data Constraint = Stretch | Natural | Ratio Float | Fixed Int
makePrisms ''Constraint

data Constraints = Constraints {width :: Int, height :: Int}

newtype Hor = Hor Vty.Image
  deriving newtype Monoid
newtype Vert = Vert Vty.Image
  deriving newtype Monoid

instance Semigroup Hor where
  Hor a <> Hor b = Hor $ a Vty.<|> b

instance Semigroup Vert where
  Vert a <> Vert b = Vert $ a Vty.<-> b

data Dir = Horizontal | Vertical
  deriving Eq

constrainWidth :: Int -> React a -> React a
constrainWidth w = adjustContext (\(Constraints _ h) -> Constraints w h)

constrainHeight :: Int -> React a -> React a
constrainHeight h = adjustContext (\(Constraints w _) -> Constraints w h)

flex :: Dir -> [(Constraint, React Vty.Image)] -> React Vty.Image
flex dir pieces = joiner <$> do
    constraints <- useConstraints
    (rs :: [Either (Constraint, React Vty.Image) Vty.Image]) <- for pieces $ \case
      (Natural, r) -> Right <$> r
      (Fixed n, r) -> do
          Right <$> constrainWidth n r
      x -> pure $ Left x
    let consumed = sumOf (folded . _Right . to measureImage) rs
    let remainder = max 0 (viewportSize constraints - consumed)
    nextRs <- for rs $ \case
      Left (Ratio n, r) -> do
          Right <$> (constrain (n `percentOf` viewportSize constraints) $ r)
      x -> do
          pure $ x
    let consumed' = sumOf (folded . _Right . to measureImage) nextRs
    let remainder' = max 0 (viewportSize constraints - consumed')
    let numStretch = lengthOf (folded . _Left . _1 . filteredBy (_Stretch)) nextRs
    for nextRs $ \case
      Left (Stretch, r) -> do
          (constrain (floor $ fromIntegral remainder' / fromIntegral numStretch) $ r)
      Left _ -> do
          error "Got unexpected sizing type"
      Right img -> do
          return img
  where
    (joiner, measureImage, constrain, viewportSize) = case dir of
        Horizontal -> (Vty.horizCat, Vty.imageWidth, constrainWidth, getWidth . fst)
        Vertical -> (Vty.vertCat, Vty.imageHeight, constrainHeight, getHeight . snd)

percentOf :: Float -> Int -> Int
percentOf perc total = floor (perc * fromIntegral total)

-- | Dynamically sized block
charBlock :: Component Char Vty.Image
charBlock = component $ \c -> do
    (Width w, Height h) <- useConstraints
    return $ Vty.charFill Vty.defAttr c w h

hundo :: Component () Vty.Image
hundo = component $ \() -> do
    flex Horizontal [(Stretch, charBlock "char" '#'), (Ratio 0.5, charBlock "char" '+'), (Stretch, charBlock "char" '&')]

centerIsh :: React Vty.Image -> Component () Vty.Image
centerIsh c = component $ \() -> do
    flex Horizontal [(Stretch, charBlock "char" '&'), (Natural, c), (Stretch, charBlock "char" '&')]
    -- flex Vertical [(Stretch, charBlock "char" '='), (Natural, inner), (Stretch, charBlock "char" '=')]

useConstraints :: React (Width Int, Height Int)
useConstraints = do
    (w, h) <- useViewport
    Constraints w' h' <- useContextWithDefault (Constraints w h)
    return (Width w', Height h')

withDir :: Dir -> Constraint -> React a -> React a
withDir d sz m = do
    c <- useConstraints
    case sz of
        (Ratio r) -> constrain (r `percentOf` size c) m
        (Fixed f) -> constrain f m
        Natural -> m
        Stretch -> m
  where
    (constrain, size) = case d of
        Horizontal -> (constrainWidth, getWidth . fst)
        Vertical -> (constrainHeight, getHeight . snd)

withHeight :: Constraint -> React a -> React a
withHeight = withDir Vertical
withWidth :: Constraint -> React a -> React a
withWidth = withDir Horizontal
