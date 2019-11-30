import Test.Tasty

import Bot.TypeTest
import ConfigTest
import Ipip.ParserTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Unit Tests"
    [ test_IpipParser
    , test_BotTypes
    , test_Config ]
