{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigTest where

import Servant.Client         

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import Bot.Types
import Config

test_Config :: TestTree
test_Config = testGroup "Config"
  [ testProperty "loadBotConfig" test_loadBotConfig ]

test_loadBotConfig :: Property
test_loadBotConfig = property do
  config <- loadBotConfig
  config === BotConfig (BaseUrl Https "recruitbot.trikeapps.com" 443 "")