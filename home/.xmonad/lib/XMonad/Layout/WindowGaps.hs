{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.WindowGaps
    ( gaps
    , Gaps
    ) where

import Graphics.X11 (Rectangle(..), Position, Dimension)
import Control.Arrow (second)
import XMonad.Util.Font (fi)
import XMonad.Layout.LayoutModifier

gaps :: Int -> l a -> ModifiedLayout Gaps l a
gaps g = ModifiedLayout (Gaps g)

data Gaps a = Gaps Int deriving (Show, Read)

instance LayoutModifier Gaps a where
    pureModifier gap r _ wrs = (fmap (second $ shrinkRect gap r) wrs, Nothing)

-- | Shrink the window's rectangle to add a nice gap between windows.
--
shrinkRect :: Gaps a -> Rectangle -> Rectangle -> Rectangle
shrinkRect (Gaps g) (Rectangle sx sy sw sh) (Rectangle x y w h) =
    let dl = gap g $ x == sx
        dt = gap g $ y == sy
        dr = gap g $ gapRight x w sx sw
        db = gap g $ gapRight y h sy sh
    in Rectangle (x + dl) (y + dt) (w - fi dl - dr) (h - fi dt - db)

gap :: Integral a => Int -> Bool -> a
gap g b = if b then fi g else truncate . (/ 2) $ (fi g :: Double)

-- | Calculate the gap's offset from the right/bottom.
--
gapRight :: Position -> Dimension -> Position -> Dimension -> Bool
gapRight x w sx sw = x + fi w == sx + fi sw
