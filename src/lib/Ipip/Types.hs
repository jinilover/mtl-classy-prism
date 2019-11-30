{-# LANGUAGE TemplateHaskell #-}

-- | Types used by 'Ipip.Parsers'
module Ipip.Types where

import Control.Lens

newtype ParseError =
  IpipParseError Text
  deriving (Show, Eq)

makeClassyPrisms ''ParseError