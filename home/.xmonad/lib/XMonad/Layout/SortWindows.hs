{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module XMonad.Layout.SortWindows
    ( sortQuery
    , setQuery
    , (<?>)
    , composeAs
    , SortLayout
    , SortMessage (..)
    ) where

import Control.Monad
import Data.List (delete, intersect, (\\))
import Data.Maybe
import Data.Monoid

import XMonad hiding (focus)
import XMonad.StackSet (Workspace (..), Stack (..))
import XMonad.Util.Invisible
import qualified XMonad.StackSet as W

type InvisibleQuery = Invisible Maybe (Query Any)

data SortMessage = SetQuery String (Query Any)
                 | ResetSort String
                 | SwapWindow
    deriving (Typeable)

instance Message SortMessage

data SortLayout l1 l2 a = SortLayout
    { focused     :: [a]
    , left        :: [a]
    , right       :: [a]
    , name        :: String
    , fill        :: Bool
    , delta       :: Rational
    , mfrac       :: Rational
    , query       :: InvisibleQuery
    , layoutLeft  :: l1 a
    , layoutRight :: l2 a
    }
    deriving (Read, Show)

infix 1 <?>
(<?>) :: (Monoid (f b), Functor f) => (a -> b) -> f a -> f b
f <?> p = fmap f p

composeAs :: (Monoid (f b), Functor f) => (a -> b) -> [f a] -> f b
composeAs f = composeAll . fmap (fmap f)

sortQuery :: (LayoutClass l1 a, LayoutClass l2 a)
             => String
             -> Bool
             -> Rational
             -> Rational
             -> Query Any
             -> l1 a
             -> l2 a
             -> SortLayout l1 l2 a
sortQuery n f d r q = SortLayout [] [] [] n f d r (I (Just q))

setQuery :: String -> Query Any -> X ()
setQuery n q = broadcastMessage $ SetQuery n q

instance (LayoutClass l1 Window, LayoutClass l2 Window) => LayoutClass (SortLayout l1 l2) Window where
    doLayout (SortLayout f w1 w2 name fill delta frac query l1 l2) r s =
        let origws = W.integrate s              -- passed in windows
            w1c = origws `intersect` w1         -- current windows in the first pane
            w2c = origws `intersect` w2         -- current windows in the second pane
            new = origws \\ (w1c <> w2c)        -- new windows
            f'  = focus s : delete (focus s) f  -- list of focused windows, contains 2 elements at most
        in do
            matching <- queryFilter query new   -- new windows matching predecate
            let w1' = w1c <> matching           -- updated first pane windows
                w2' = w2c <> (new \\ matching)  -- updated second pane windows
                s1  = differentiate f' w1'      -- first pane stack
                s2  = differentiate f' w2'      -- second pane stack
            (wrs, ml1', ml2') <- split fill w1' l1 s1 w2' l2 s2 frac r
            return (wrs, Just $ SortLayout f' w1' w2' name fill delta frac query (fromMaybe l1 ml1') (fromMaybe l2 ml2'))
      where
        queryFilter (I (Just q)) ws = filterM (fmap getAny . runQuery q) ws
        queryFilter (I Nothing)  _  = return []

    handleMessage sl@(SortLayout _ _ _ name _ delta frac _ _ _) m
        | Just Shrink <- fromMessage m =
            let frac' = max 0 $ frac - delta
            in return . Just $ sl { mfrac = frac' }
        | Just Expand <- fromMessage m =
            let frac' = min 1 $ frac + delta
            in return . Just $ sl { mfrac = frac' }
        | Just (SetQuery n q) <- fromMessage m =
            if n == name
                then return . Just $ sl { query = I (Just q) }
                else passThroughMessage sl m
        | Just (ResetSort n) <- fromMessage m =
            if n == name
                then return . Just $ sl { focused = [], left = [], right = [] }
                else passThroughMessage sl m
        | Just SwapWindow <- fromMessage m = swap sl m
        | otherwise = passThroughMessage sl m

    description sl =
        unwords [ "SortLayout", description (layoutRight sl), description (layoutRight sl) ]

swap (SortLayout f ws1 ws2 name fill delta frac query l1 l2) _ = do
    mst <- gets $ W.stack . W.workspace . W.current . windowset
    let (ws1', ws2') = case mst of
                          Nothing -> (ws1, ws2)
                          Just st | foc `elem` ws1 -> (foc `delete` ws1, foc : ws2)
                                  | foc `elem` ws2 -> (foc : ws1, foc `delete` ws2)
                                  | otherwise      -> (ws1, ws2)
                            where foc = W.focus st
    if (ws1, ws2) == (ws1', ws2')
        then return Nothing
        else return . Just $ SortLayout f ws1' ws2' name fill delta frac query l1 l2

passThroughMessage (SortLayout f ws1 ws2 name fill delta frac query l1 l2) m = do
    ml1' <- handleMessage l1 m
    ml2' <- handleMessage l2 m
    if isJust ml1' || isJust ml2'
        then return . Just $ SortLayout f ws1 ws2 name fill delta frac query (fromMaybe l1 ml1') (fromMaybe l2 ml2')
        else return Nothing

split True _  l1 s1 [] _  _  _ r = runLayout (Workspace "" l1 s1) r >>= \(wrs, ml) -> return (wrs, ml, Nothing)
split _    [] _  _  _  l2 s2 _ r = runLayout (Workspace "" l2 s2) r >>= \(wrs, ml) -> return (wrs, Nothing, ml)
split _    _  l1 s1 _  l2 s2 f r = do
    (wrs1, ml1') <- runLayout (Workspace "" l1 s1) r1
    (wrs2, ml2') <- runLayout (Workspace "" l2 s2) r2
    return (wrs1 <> wrs2, ml1', ml2')
  where
    (r1, r2) = splitHorizontallyBy f r

differentiate :: Eq q => [q] -> [q] -> Maybe (Stack q)
differentiate (z:zs) xs
    | z `elem` xs = Just Stack { focus = z
                               , up    = reverse $ takeWhile (/= z) xs
                               , down  = tail $ dropWhile (/= z) xs }
    | otherwise   = differentiate zs xs
differentiate [] xs = W.differentiate xs
