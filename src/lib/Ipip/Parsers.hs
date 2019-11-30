{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Functions for parsing the html file of the personality test result.
module Ipip.Parsers 
  ( ParseError(..)
  , AsParseError(..)
  , parseIpipFile
  , parseBigFiveResult
  , parseDomains
  , parseDomain
  , parseFacet )
  where

import Control.Lens
import Control.Monad.Except (liftEither)
import Data.Attoparsec.Text (Parser, takeWhile1, skipWhile, decimal, parseOnly, string, endOfInput)
import Text.HTML.TagSoup    (Tag(..), (~/=), parseTags)

import qualified Data.Text.IO as TIO

import Bot.Types
import Ipip.Types

-- | Parses the html file to 'BigFiveResult' to be encoded to JSON value 
-- as required by the Recruitbot API.
-- It reads the html file of the provided file name, 
-- parses the html content into '[Tag Text]', 
-- parses '[Tag Text]' to 'BigFiveResult'
parseIpipFile 
  :: (MonadError e m, AsParseError e, MonadIO m) 
  => UserInfo -> m BigFiveResult
parseIpipFile userInfo@UserInfo{..} = do
  textData <- liftIO . TIO.readFile . toS $ _userIpipFile
  parseBigFiveResult userInfo $ parseTags textData

-- | Parses the '[Tag Text]' to 'BigFiveResult'.
-- It parses '[Tag Text]' to domains of corresponding facets informations,
-- uses the given 'UserInfo' together with the parsed '[Domain]' to create 'BigFiveResult'.
parseBigFiveResult
  :: (MonadError e m, AsParseError e)
  => UserInfo -> [Tag Text] -> m BigFiveResult
parseBigFiveResult UserInfo{..} tags = do
  liftEither . mkResult . parseDomains $ tags
  where
    mkResult domains
      | length domains == 5 = Right $ BigFiveResult _userName _userEmail domains
      | otherwise           = Left (_IpipParseError # "5 domains expected from the html data")

-- | Parses the '[Tag Text]' to domains of corresponding facet information.
-- Since the facet information of each domain is enclosed by '<div class="graph-txt"></div>' only,
-- it only pick those '[Tag Text]' bounded by these 2 tags for the domain facet information.
parseDomains :: [Tag Text] -> [Domain]
parseDomains = rights . toEithers
  where 
    toEithers tags =  case dropWhile (~/= openTag) tags of
                        _ : tail' -> parseDomain (takeWhile (~/= closeTag) tail') : toEithers tail'
                        _         -> []
    openTag :: Tag Text
    openTag = TagOpen "div" [("class","graph-txt")]
    closeTag :: Tag Text
    closeTag = TagClose "div"

-- | Parses the '[Tag Text]' to domain facet information.
-- It parses each 'Tag Text' for facet information.  
-- If there is no facet parsed, it will treat the parsing as failed.
-- If there is multiple facets information, the first one will be regarded as the domain overall score.
parseDomain :: [Tag Text] -> Either Text Domain
parseDomain tags =  case (rights . fmap parseFacet) tags of
                      Facet{..} : tail' -> return $ Domain _facetName _facetScore tail'
                      _                 -> Left "No facet information to build a domain score"

-- | Parses the 'Tag Text' to facet information.
-- 'Tag' has several data constructors and facet is only available from 'TagText' constructor,
-- it will only parse the text within 'TagText' for the facet information.
parseFacet :: Tag Text -> Either Text Facet
parseFacet (TagText s) = first toS $ parseOnly facetParser s
parseFacet _ = Left "Facet information only comes from TagText"

-- | A parser that parses the facet information from a text.
facetParser :: Parser Facet
facetParser = do
  _facetName  <- takeWhile1 (/= '.')
  _           <- skipWhile (== '.')
  _facetScore <- decimal
  _           <- string " " <|> string ""
  _           <- endOfInput
  return Facet{..}
