{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Functions for making http request to the API
module Bot.Client 
  ( mkBotEnv
  , submitBigFive )
where

import Control.Lens
import Control.Monad.Except       (liftEither)
import Data.Proxy                 (Proxy)
import Network.HTTP.Client        (newManager)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Network.HTTP.Types         (status404)
import Servant.API                (ReqBody, Post, JSON, PlainText, (:>))
import Servant.Client             (ClientM, ClientError(..), mkClientEnv, client, runClientM, responseStatusCode)
  
import Bot.Types

-- | Create an environment that enables a request be sent to the API
-- It uses the API endpoint configured in the config file to create the environment
mkBotEnv :: MonadIO m => BotConfig -> m BotEnv
mkBotEnv BotConfig{..} = liftIO $
  newManager tlsManagerSettings <&> (BotEnv . flip mkClientEnv _botEndpoint)

-- | Makes a POST request to send 'BigFiveResult' in the required JSON format
-- to the API.
submitBigFive 
  :: (MonadReader BotEnv m, MonadError e m, AsHttpError e, MonadIO m)
  => BigFiveResult -> m Text
submitBigFive payload = do
  BotEnv{..} <- ask
  response <- (liftIO . flip runClientM _botClientEnv . submitBigFiveCM) payload
  liftEither . first mapError $ response
  where
    mapError err@(FailureResponse _ resp) 
      | responseStatusCode resp == status404 = _HttpNotFound # ()
      | otherwise = _HttpClientError # err
    mapError err = _HttpClientError # err

-- | Data type and function for handling the http request by using servant client
-- To separate the concern, they are not exposed in this module
type SubmitBigFiveApi 
  =  "api"
  :> "v1"
  :> "roles"
  :> "bellroy-tech-team-recruit"
  :> "big_five_profile_submissions"
  :> ReqBody '[JSON] BigFiveResult
  :> Post '[PlainText] Text

api :: Proxy SubmitBigFiveApi
api = Proxy

submitBigFiveCM :: BigFiveResult -> ClientM Text
submitBigFiveCM = client api