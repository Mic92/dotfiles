{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.GuardLayout
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <simongmzlj@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Configure layouts on a conditional basis: use layouts and apply
-- layout modifiers selectively based on arbitrary run-time conditions.
-----------------------------------------------------------------------------

module XMonad.Layout.GuardLayout
    ( -- * Usage
      -- $usage
      GuardLayout
    , Condition
    , validate
    , onCondition
    , onConditions
    , modCondition
    , modConditions
    ) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module by importing it into your \~/.xmonad/xmonad.hs file:
--
-- > import Xmonad.Layout.GuardLayout
-- > import Xmonad.Layout.GuardLayout.Instances
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook =
--
-- Note that @l1@, @l2@, and @l3@ can be arbitrarily complicated
-- layouts,
--

-- | Structure for representing a condition-based layout. We store
--   a set of conditions to match against and the two layouts. We save
--   the layout choice in the Bool to be used to implement
--   description.
data GuardLayout p l1 l2 a = GuardLayout [p] Bool (l1 a) (l2 a)
    deriving (Read, Show)

-- | Every condition must be an instance of 'Condition' which defines
-- one method, validate, which evaluates the conditional.
class (Show p, Read p) => Condition p where

    -- | Validate the condition.
    validate :: W.Workspace WorkspaceId l a -> p -> X Bool
    validate _ _ = return False

-- | Specify one layout to use when a particular condition is met, and
--   another to fall back onto otherwise.
onCondition :: (LayoutClass l1 a, LayoutClass l2 a, Condition p)
               => p     -- ^ the condition
               -> l1 a  -- ^ layout to use when condition is true
               -> l2 a  -- ^ layout to use otherwise.
               -> GuardLayout p l1 l2 a
onCondition p = onConditions [p]

-- | Specify one layout to use when a particular set of conditions are
--   met, and another to fall back onto otherwise.
onConditions :: (LayoutClass l1 a, LayoutClass l2 a, Condition p)
                => [p]   -- ^ the set of conditions
                -> l1 a  -- ^ layout to use when the conditions are true
                -> l2 a  -- ^ layout to use otherwise.
                -> GuardLayout p l1 l2 a
onConditions p = GuardLayout p False

-- | Specify a layout modifier to apply to a particular workspace if
--   and only if the condition is met.
modCondition :: (LayoutClass l a, Condition p)
                => p                               -- ^ the condition
                -> (l a -> ModifiedLayout lm l a)  -- ^ the modifier to apply when the condition is true
                -> l a                             -- ^ the base layout
                -> GuardLayout p (ModifiedLayout lm l) l a
modCondition p = modConditions [p]

-- | Specify a layout modifier to apply to a particular workspace if
--   and only if a set of conditions are met.
modConditions :: (LayoutClass l a, Condition p)
                 => [p]                             -- ^ the set of conditions
                 -> (l a -> ModifiedLayout lm l a)  -- ^ the modifier to apply when the conditions are true
                 -> l a                             -- ^ the base layout
                 -> GuardLayout p (ModifiedLayout lm l) l a
modConditions p f l = GuardLayout p False (f l) l

instance (Condition p, LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (GuardLayout p l1 l2) a where
    runLayout ws@(W.Workspace i p@(GuardLayout ps _ lt lf) ms) r =
        checkCondition ws ps >>=
        \b -> if b then do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                           return (wrs, Just $ mkNewPerScreenT p mlt')
                   else do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                           return (wrs, Just $ mkNewPerScreenF p mlt')

    handleMessage (GuardLayout ps bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (return . Just . flip (GuardLayout ps bool) lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (return . Just . GuardLayout ps bool lt)

    description (GuardLayout _ True l1 _) = description l1
    description (GuardLayout _ _    _ l2) = description l2

-- | Check if any of the conditions are true.
checkCondition :: (Condition p) => W.Workspace WorkspaceId l a -> [p] -> X Bool
checkCondition ws = foldM (\a p -> (a ||) <$> validate ws p) False

-- | Construct new GuardLayout values with possibly modified layouts.
mkNewPerScreenT :: (Condition p) => GuardLayout p l1 l2 a -> Maybe (l1 a) -> GuardLayout p l1 l2 a
mkNewPerScreenT (GuardLayout ps _ lt lf) mlt' = flip (GuardLayout ps True) lf $ fromMaybe lt mlt'

mkNewPerScreenF :: (Condition p) => GuardLayout p l1 l2 a -> Maybe (l2 a) -> GuardLayout p l1 l2 a
mkNewPerScreenF (GuardLayout ps _ lt lf) mlf' = GuardLayout ps False lt $ fromMaybe lf mlf'
