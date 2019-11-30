{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

import Bot.Types
import Ipip.Types

newtype OptError = 
  OptParseError Text
  deriving (Show, Eq)

makeClassyPrisms ''OptError

data AppError
  = ParseAppError ParseError
  | HttpAppError HttpError
  | OptAppError OptError
  deriving (Show, Eq)

makeClassyPrisms ''AppError

instance AsParseError AppError where
  _ParseError = _ParseAppError

instance AsHttpError AppError where
  _HttpError = _HttpAppError

instance AsOptError AppError where
  _OptError = _OptAppError