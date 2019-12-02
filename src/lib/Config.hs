{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Types and functions for loading configuration information 
-- of API endpoint from Dhall file
module Config 
  ( loadBotConfig )
where

import Control.Monad.Catch  (MonadThrow, throwM)
import Dhall                (Interpret, input, auto)
import Servant.Client       (parseBaseUrl)

import Bot.Types
import Paths_mtl_classy_prism    (getDataFileName)

newtype ConfigException =
  ConfigException Text
  deriving (Show, Eq)

instance Exception ConfigException

-- | Loads configuration from dhall file 
-- , make sure the API endpoint is a valid url.
loadBotConfig :: (MonadThrow m, MonadIO m) => m BotConfig
loadBotConfig = do
  filePath <- liftIO (getDataFileName "application.dhall")
  dhallConfig <- (liftIO . input auto . toS) filePath
  either (throwM . ConfigException) return $ validateConfig dhallConfig

validateConfig :: DhallBotConfig -> Either Text BotConfig
validateConfig DhallBotConfig{..} = 
  BotConfig <$> (first show . parseBaseUrl . toS) endpoint

data DhallBotConfig = 
  DhallBotConfig
  { endpoint :: Text }
  deriving ( Show, Generic, Interpret)