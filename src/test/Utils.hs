{-# LANGUAGE RecordWildCards #-}

-- | Utility functions that create data from tuple list 
-- and shared by multiple tests
module Utils where

import Bot.Types
import Text.Email.Validate  (EmailAddress)

mkBigFiveResult :: Text -> EmailAddress -> [NonEmpty (Text, Int)] -> BigFiveResult
mkBigFiveResult _name _email nameScorePairs = 
  let _domains = fmap mkDomain nameScorePairs
  in  BigFiveResult{..}

mkDomain :: NonEmpty (Text, Int) -> Domain
mkDomain ((_domainName, _domainScore) :| facets) = 
  let _domainFacets = uncurry Facet <$> facets in Domain{..}
