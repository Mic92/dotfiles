{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances, FlexibleContexts, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Minimize
-- Copyright   :  (c) Jan Vornberger 2009, Alejandro Serrano 2010
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- Makes it possible to minimize windows, temporarily removing them
-- from the layout until they are restored.
--
-----------------------------------------------------------------------------

module XMonad.Layout.MinimizePlus (
        -- * Usage
        -- $usage
        minimize,
        minimizeWindow,
        MinimizeMsg(..),
        Minimize,
    ) where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.List
import Foreign.C.Types (CLong)
import XMonad
import XMonad.Layout.BoringWindows as BW
import XMonad.Layout.LayoutModifier
import XMonad.Util.WindowProperties (getProp32)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

data Minimize a = Minimize [Window] (M.Map Window W.RationalRect) deriving ( Read, Show )

minimize :: LayoutClass l Window => l Window -> ModifiedLayout Minimize l Window
minimize = ModifiedLayout $ Minimize [] M.empty

data MinimizeMsg = MinimizeWin Window
                 | MinimizeFloating
                 | RestoreMinimizedWin Window
                 | RestoreNextMinimized
                 | RestoreAll
                 deriving (Typeable, Eq)

instance Message MinimizeMsg

minimizeWindow :: Window -> X ()
minimizeWindow w = sendMessage (MinimizeWin w) >> BW.focusDown

setMinimizedState :: Window -> Int -> (CLong -> [CLong] -> [CLong]) -> X ()
setMinimizedState win st f = do
    setWMState win st
    withDisplay $ \dpy -> do
        state  <- getAtom "_NET_WM_STATE"
        mini   <- getAtom "_NET_WM_STATE_HIDDEN"
        wstate <- fromMaybe [] <$> getProp32 state win
        let ptype   = 4 -- The atom property type for changeProperty
            fi_mini = fromIntegral mini
        io . changeProperty32 dpy win state ptype propModeReplace $ f fi_mini wstate

setMinimized :: Bool -> Window -> X ()
setMinimized True  win = setMinimizedState win iconicState (:)
setMinimized False win = setMinimizedState win normalState delete

instance LayoutModifier Minimize Window where
    modifierDescription _ = "Minimize"

    modifyLayout (Minimize minimized _) wksp rect = do
        let stack = W.stack wksp
            filt  = stack >>= W.filter (`notElem` minimized)
        runLayout (wksp { W.stack = filt }) rect

    handleMess (Minimize minimized unfloated) m
        | Just (MinimizeWin w) <- fromMessage m, w `notElem` minimized = do
            setMinimized True w
            floats <- gets $ W.floating . windowset
            case M.lookup w floats of
                Nothing -> return . Just $ Minimize (w : minimized) unfloated
                Just r  -> do
                    modifyWindowset $ W.sink w
                    return . Just $ Minimize (w : minimized) (M.insert w r unfloated)

        | Just MinimizeFloating <- fromMessage m = do
            floats <- gets $ W.floating . windowset
            mapM_ (setMinimized True) $ M.keys floats
            modifyWindowset $ \s -> s { W.floating = M.empty }
            return . Just $ Minimize (M.keys floats `mappend` minimized) (floats `M.union` unfloated)

        | Just RestoreAll <- fromMessage m = do
            mapM_ (setMinimized False) minimized
            -- modifyWindowset $ W.focusWindow (head minimized)
            modifyWindowset $ \s -> s { W.floating = W.floating s `M.union` unfloated }
            return . Just $ Minimize [] M.empty

        | Just (RestoreMinimizedWin w) <- fromMessage m = restore w minimized unfloated
        | Just RestoreNextMinimized    <- fromMessage m =
            if null minimized
                then return Nothing
                else restore (head minimized) minimized unfloated

        | Just BW.UpdateBoring <- fromMessage m = do
            ws <- gets $ W.workspace . W.current . windowset
            flip sendMessageWithNoRefresh ws $ BW.Replace "Minimize" minimized
            return Nothing

        | otherwise = return Nothing

restore :: Window -> [Window] -> M.Map Window W.RationalRect -> X (Maybe (Minimize a))
restore w minimized unfloated = do
    setMinimized False w
    case M.lookup w unfloated of
        Nothing ->
            -- modifyWindowset $ W.focusWindow w
            return . Just $ Minimize (w `delete` minimized) unfloated
        Just r  -> do
            -- modifyWindowset $ W.focusWindow w . W.float w r
            modifyWindowset $ W.float w r
            return . Just $ Minimize (w `delete` minimized) (w `M.delete` unfloated)

modifyWindowset :: (WindowSet -> WindowSet) -> X ()
modifyWindowset f = modify $ \s -> s { windowset = f (windowset s) }
