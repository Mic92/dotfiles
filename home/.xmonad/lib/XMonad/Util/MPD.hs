{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses #-}

module XMonad.Util.MPD where

import Control.Monad
import qualified Network.MPD as MPD

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import qualified XMonad.Util.ExtensibleState as XS

data MPDConf = MPDConf
    { host :: Maybe String
    , port :: Maybe String
    } deriving (Read, Show, Typeable)

instance ExtensionClass MPDConf where
    initialValue = MPDConf Nothing Nothing
    extensionType = PersistentExtension

changeHost :: XPConfig -> X ()
changeHost conf = inputPromptWithCompl conf "MPD_HOST" (historyCompletionP (== "MPD_HOST: "))
    ?+ setHost . \x -> guard (not $ null x) >> return x

setHost :: Maybe String -> X ()
setHost h = XS.modify $ \conf -> conf { host = h }

getHost :: X (Maybe String)
getHost = XS.gets host

withMPD :: MPD.MPD a -> X ()
withMPD cmd = XS.get >>= flip (liftM2 runMPD host port) cmd

runMPD :: Maybe String -> Maybe String -> MPD.MPD a -> X ()
runMPD host port cmd = io . void $ MPD.withMPD_ host port cmd
