{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module React.Style (
    flex
                   , vCenter
                   , hCenter
                   , center
                   , useConstraints
                   , constrainWidth
                   , constrainHeight
                   , adjustWidth
                   , adjustHeight
                   , padG
                   , padLeft
                   , padRight
                   , padTop
                   , padBottom
                   , padAll
                   )
                     where

import React
import React.CoreHooks
import React.Vty
import qualified Graphics.Vty as Vty
import Data.Traversable
import Data.Monoid
import Control.Lens
import Lib
import Data.Maybe

newtype Width a = Width {getWidth :: a}
  deriving Show
newtype Height a = Height {getHeight :: a}
  deriving Show

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
constrainWidth w = adjustWidth (const w)

constrainHeight :: Int -> React a -> React a
constrainHeight h = adjustHeight (const h)

adjustWidth :: (Int -> Int) -> React a -> React a
adjustWidth f m = do
    (Width w, Height h) <- useConstraints
    withContext (Constraints (f w) h) m

adjustHeight :: (Int -> Int) -> React a -> React a
adjustHeight f m = do
    (Width w, Height h) <- useConstraints
    withContext (Constraints w (f h)) m

flex :: Dir -> [(Constraint, React Vty.Image)] -> React Vty.Image
flex dir pieces = joiner <$> do
    useConstraints >>= \case
      constraints -> do
          (rs :: [Either (Constraint, React Vty.Image) Vty.Image]) <- for pieces $ \case
            (Natural, r) -> Right <$> r
            (Fixed n, r) -> do
                Right <$> constrainPrimary n r
            x -> pure $ Left x
          let consumed = sumOf (folded . _Right . to measureImagePrimary) rs
          let remainder = max 0 (viewportSizePrimary constraints - consumed)
          nextRs <- for rs $ \case
            Left (Ratio n, r) -> do
                Right <$> (constrainPrimary (n `percentOf` viewportSizePrimary constraints) $ r)
            x -> do
                pure $ x
          let maxSecondary = fromMaybe (viewportSizeSecondary constraints) $ maximumOf (folded . _Right . to measureImageSecondary) nextRs
          let consumed' = sumOf (folded . _Right . to measureImagePrimary) nextRs
          let remainder' = max 0 (viewportSizePrimary constraints - consumed')
          let numStretch = lengthOf (folded . _Left . _1 . filteredBy (_Stretch)) nextRs
          for nextRs $ \case
            Left (Stretch, r) -> do
                (constrainSecondary maxSecondary . constrainPrimary (floor $ fromIntegral remainder' / fromIntegral numStretch) $ r)
            Left _ -> do
                error "Got unexpected sizing type"
            Right img -> do
                return img
  where
    (joiner, measureImagePrimary, measureImageSecondary, constrainPrimary, constrainSecondary, viewportSizePrimary, viewportSizeSecondary) = case dir of
        Horizontal -> (Vty.horizCat, Vty.imageWidth, Vty.imageHeight, constrainWidth, constrainHeight, getWidth . fst, getHeight . snd)
        Vertical -> (Vty.vertCat, Vty.imageHeight, Vty.imageWidth, constrainHeight, constrainWidth, getHeight . snd, getWidth . fst)

percentOf :: Float -> Int -> Int
percentOf perc total = floor (perc * fromIntegral total)

-- | Dynamically sized block
charBlock :: Component Char Vty.Image
charBlock = component $ \c -> do
    (Width w, Height h) <- useConstraints
    return $ Vty.charFill Vty.defAttr c w h

-- centerIsh :: React Vty.Image -> Component () Vty.Image
-- centerIsh c = component $ \() ->  do
--     let inner = constrainHeight 1 $ flex Horizontal [(Stretch, charBlock "char" ' '), (Natural, c), (Stretch, charBlock "char" ' ')]
--     flex Vertical [(Stretch, charBlock "char" ' '), (Natural, inner), (Stretch, charBlock "char" ' ')]

gCenter :: Dir -> React Vty.Image -> React Vty.Image
gCenter dir imgM = do
    flex dir [(Stretch, charBlock "char" ' '), (Natural, imgM), (Stretch, charBlock "char" ' ')]

vCenter :: React Vty.Image -> React Vty.Image
vCenter = gCenter Vertical
hCenter :: React Vty.Image -> React Vty.Image
hCenter = gCenter Horizontal
center :: React Vty.Image -> React Vty.Image
center = vCenter . hCenter


useConstraints :: React (Width Int, Height Int)
useConstraints = do
    (w, h) <- useViewport
    Constraints w' h' <- useContextWithDefault (Constraints w h)
    return (Width w', Height h')

withDir :: Dir -> Constraint -> React a -> React a
withDir d sz m = do
    useContext >>= \case
        Nothing -> m
        Just c -> do
            case sz of
                (Ratio r) -> constrain (r `percentOf` size c) m
                (Fixed f) -> constrain f m
                Natural -> m
                Stretch -> m
  where
    (constrain, size) = case d of
        Horizontal -> (constrainWidth, width)
        Vertical -> (constrainHeight, height)

withHeight :: Constraint -> React a -> React a
withHeight = withDir Vertical
withWidth :: Constraint -> React a -> React a
withWidth = withDir Horizontal


data Side = LeftSide | RightSide | TopSide | BottomSide
  deriving (Show, Eq)

padG :: Char -> Side -> Int -> React Vty.Image -> React Vty.Image
padG c s n imgM = do
    case s of
        LeftSide -> adjustWidth (subtract 1) $ do
            img <- imgM
            return $ Vty.charFill Vty.defAttr c n (Vty.imageHeight img) Vty.<|> img
        RightSide -> adjustWidth (subtract 1) $ do
            img <- imgM
            return $ img Vty.<|> Vty.charFill Vty.defAttr c n (Vty.imageHeight img)
        TopSide -> adjustHeight (subtract 1) $ do
            img <- imgM
            return $ Vty.charFill Vty.defAttr c (Vty.imageWidth img) n Vty.<-> img
        BottomSide -> adjustHeight (subtract 1) $ do
            img <- imgM
            return $ img Vty.<-> Vty.charFill Vty.defAttr c (Vty.imageWidth img) n

padLeft, padRight, padTop, padBottom, padAll :: Int -> React Vty.Image -> React Vty.Image
padLeft = padG ' ' LeftSide
padRight = padG ' ' RightSide
padTop = padG ' ' TopSide
padBottom = padG ' ' BottomSide
padAll n = padLeft n . padRight n . padTop n . padBottom n
