{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Exception
import qualified Data.Configurator as C
import Data.Configurator.Types (Config, Value)

getConfig :: IO Config
getConfig = do
  cfg <-
    try (C.load [C.Required "$(HOME)/.config/hidd/hiddrc"]) :: IO (Either SomeException Config)
  case cfg of
    Left ex -> return C.empty
    Right a -> return a

getGitlabKey :: IO (Maybe String)
getGitlabKey = do
  config <- getConfig
  gitKey <- C.lookup config "gitlab-private-key"
  return gitKey
