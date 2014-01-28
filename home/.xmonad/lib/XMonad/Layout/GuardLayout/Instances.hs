-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.GuardLayout.Instances
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <simongmzlj@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Configure layouts on a conditional basis: use layouts and apply
-- layout modifiers selectively based on arbitrary run-time conditions.
-----------------------------------------------------------------------------

module XMonad.Layout.GuardLayout.Instances
    ( ScreenSpace (..)
    , ScreenSize (..)
    , AspectRatio (..)
    , Hostname (..)
    , ifWider
    , ifTaller
    , ifHostname
    , whenWider
    , whenTaller
    , whenHostname
    ) where

import Control.Applicative
import Data.Maybe
import Data.Ratio
import System.Posix.Unistd (getSystemID, nodeName)

import Graphics.X11.Xinerama (getScreenInfo)

import XMonad
import XMonad.Layout.GuardLayout
import XMonad.Layout.LayoutModifier

-- |
ifWider :: (LayoutClass l1 a, LayoutClass l2 a)
           => Dimension  -- ^ the target screen width
           -> l1 a       -- ^ the layout to use when the screen is wide enough
           -> l2 a       -- ^ the layout to use otherwise
           -> GuardLayout ScreenSize l1 l2 a
ifWider w = onCondition . AtLeast $ ScreenSpace (Just w) Nothing

-- |
ifTaller :: (LayoutClass l1 a, LayoutClass l2 a)
            => Dimension  -- ^ the target screen height
            -> l1 a       -- ^ the layout to use when the screen is tall enough
            -> l2 a       -- ^ the layout to use otherwise
            -> GuardLayout ScreenSize l1 l2 a
ifTaller h = onCondition . AtLeast $ ScreenSpace Nothing (Just h)

-- |
ifHostname :: (LayoutClass l1 a, LayoutClass l2 a)
              => String  -- ^ the hostname to match
              -> l1 a    -- ^ the layout to use when the hostname matches
              -> l2 a    -- ^ the layout to use otherwise
              -> GuardLayout Hostname l1 l2 a
ifHostname = onCondition . Hostname

-- |
whenWider :: LayoutClass l a
             => Dimension                       -- ^ the target screen width
             -> (l a -> ModifiedLayout lm l a)  -- ^ the layout modifier to apply when the screen is wide enough
             -> l a                             -- ^ the base layout
             -> GuardLayout ScreenSize (ModifiedLayout lm l) l a
whenWider w = modCondition . AtLeast $ ScreenSpace (Just w) Nothing

-- |
whenTaller :: LayoutClass l a
             => Dimension                       -- ^ the target screen height
             -> (l a -> ModifiedLayout lm l a)  -- ^ the layout modifier to apply when the screen is tall enough
             -> l a                             -- ^ the base layout
             -> GuardLayout ScreenSize (ModifiedLayout lm l) l a
whenTaller h = modCondition . AtLeast $ ScreenSpace Nothing (Just h)

-- |
whenHostname :: LayoutClass l a
                => String                          -- ^ the hostname to match
                -> (l a -> ModifiedLayout lm l a)  -- ^ the layout modifier to apply when the hostname matches
                -> l a                             -- ^ the base layout
                -> GuardLayout Hostname (ModifiedLayout lm l) l a
whenHostname = modCondition . Hostname

-- | Structure representing target screen space.
data ScreenSpace = ScreenSpace (Maybe Dimension) (Maybe Dimension)
    deriving (Show, Read)

-- | Conditional for dealing with screen space or real estate.
data ScreenSize = AtLeast ScreenSpace
                | SmallerThan ScreenSpace
    deriving (Show, Read)

-- | Conditional for matching aspect ratio.
data AspectRatio = AspectRatio (Ratio Dimension)
    deriving (Show, Read)

-- | Conditional for matching hostname.
data Hostname = Hostname String
    deriving (Show, Read)

instance Condition ScreenSize where
    validate _ (SmallerThan si) =
        calculateBox si (<) <$> getScreenSize

    validate _ (AtLeast si) =
        calculateBox si (>=) <$> getScreenSize

instance Condition AspectRatio where
    validate _ (AspectRatio r) =
        getScreenSize >>= \(Rectangle _ _ sw sh) -> return $ r == (sw % sh)

instance Condition Hostname where
    validate _ (Hostname n) =
        (n ==) . nodeName <$> io getSystemID

getScreenSize :: X Rectangle
getScreenSize = io . fmap head $ getScreenInfo =<< openDisplay ""

calculateBox :: ScreenSpace -> (Dimension -> Dimension -> Bool) -> Rectangle -> Bool
calculateBox (ScreenSpace w h) op (Rectangle _ _ sw sh) =
    let w' = fromMaybe False $ op sw <$> w
        h' = fromMaybe False $ op sh <$> h
    in w' || h'
