{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types for sending the personality test result to the Recruitbot API.
module Bot.Types where

import Control.Lens hiding    ((.=))
import Data.Aeson
import Data.Aeson.Encoding    (pair)
import Servant.Client         (BaseUrl, ClientError, ClientEnv)
import Text.Email.Validate    (EmailAddress)
import Text.Email.Aeson.Instances()

-- | Possible errors encountered in sending data to the API.
data HttpError
  = HttpNotFound
  | HttpClientError ClientError 
  deriving ( Show, Eq )

makeClassyPrisms ''HttpError

-- | Information related to the user but not obtained from the test result
data UserInfo =
  UserInfo
    { _userName :: Text
    , _userEmail :: EmailAddress
    , _userIpipFile :: Text}
  deriving (Show, Eq)

-- | The API endpoint
data BotConfig = 
  BotConfig
    { _botEndpoint :: BaseUrl }
  deriving (Eq, Show)

-- | The environment acquired to connect the API endpoint
newtype BotEnv =
  BotEnv
    { _botClientEnv :: ClientEnv }

-- | This type holds the facet information parsed from 
-- the personality test result.  
-- It is part of the JSON data to be sent to the API.
data Facet =
  Facet
    { _facetName :: Text
    , _facetScore :: Int }
  deriving ( Show, Eq, Generic )

instance ToJSON Facet

-- | This type holds all the facet information of a particular domain.
-- It is part of the JSON data to be sent to the API.
data Domain =
  Domain
    { _domainName :: Text
    , _domainScore :: Int
    , _domainFacets :: [Facet] }
  deriving ( Show, Eq, Generic )

instance ToJSON Domain where
  -- | Encode 'Domain' as JSON data according to the Recruitbot API JSON specification.
  toEncoding Domain{..} = pairs $
    "Overall Score" .= _domainScore <> 
    pair "Facets" (makeEncoding _domainFacets)
    where 
      makeEncoding = pairs . flip foldl mempty \series Facet{..} -> series <> _facetName .= _facetScore
    
-- | This type holds all the information required to make a POST request to the API.
data BigFiveResult =
  BigFiveResult
    { _name :: Text
    , _email :: EmailAddress
    , _domains :: [Domain] }
  deriving ( Show, Eq, Generic )

instance ToJSON BigFiveResult where
  toEncoding BigFiveResult{..} = pairs $
    "NAME" .= _name <>
    "EMAIL" .= _email <>
    mkSeries _domains
    where 
      mkSeries = flip foldl mempty \series d@Domain{..} -> series <> pair _domainName (toEncoding d)