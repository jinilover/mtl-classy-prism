-- | Test the Json value of the personality result conform to the Recruitbot API requirement
module Bot.TypeTest where

import Data.Aeson
import Text.Email.Validate

import Bot.Types
import Utils      (mkDomain, mkBigFiveResult)

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
  
test_BotTypes :: TestTree
test_BotTypes = testGroup "Bot Types"
  [ testGroup "Bot Types Json encoding"
    [ testProperty "encode Domain" test_encode_Domain
    , testProperty "encode BigFiveResult" test_encode_BigFiveResult ]
  ]

test_encode_Domain :: Property
test_encode_Domain = property do
  let scores = ("EXTRAVERSION",65) :| [("Friendliness",49), ("Gregariousness",51), ("Assertiveness",64), ("Activity Level",78), ("Excitement-Seeking",59), ("Cheerfulness",60)]
      domain = mkDomain scores
  encode domain === "{\"Overall Score\":65,\"Facets\":{\"Friendliness\":49,\"Gregariousness\":51,\"Assertiveness\":64,\"Activity Level\":78,\"Excitement-Seeking\":59,\"Cheerfulness\":60}}"

test_encode_BigFiveResult :: Property
test_encode_BigFiveResult = property do
  let userName = "Lambda"
      userEmail = unsafeEmailAddress "lambda" "category.org"
      scoresOfDomains = 
        [ ("EXTRAVERSION",65) :| [("Friendliness",49), ("Gregariousness",51), ("Assertiveness",64), ("Activity Level",78), ("Excitement-Seeking",59), ("Cheerfulness",60)]
        , ("AGREEABLENESS",33) :| [("Trust",52), ("Morality",31), ("Altruism",34), ("Cooperation",62), ("Modesty",25), ("Sympathy",39)]
        , ("CONSCIENTIOUSNESS",65) :| [("Self-Efficacy",58), ("Orderliness",87), ("Dutifulness",45), ("Achievement-Striving",60), ("Self-Discipline",46), ("Cautiousness",46)]
        , ("NEUROTICISM",57) :| [("Anxiety",76), ("Anger",65), ("Depression",40), ("Self-Consciousness",39), ("Immoderation",50), ("Vulnerability",59)]
        , ("OPENNESS TO EXPERIENCE",55) :| [("Imagination",26), ("Artistic Interests",61), ("Emotionality",19), ("Adventurousness",89), ("Intellect",51), ("Liberalism",60)] ]
      bigFiveResult = mkBigFiveResult userName userEmail scoresOfDomains
  encode bigFiveResult === expectedEncoding bigFiveResult
  where
    expectedEncoding BigFiveResult{..} = "{\"NAME\":" <> encode _name <> 
                                          ",\"EMAIL\":" <> encode _email <> 
                                          encodeDomains _domains <> "}"
    encodeDomains = flip foldl mempty \b a@Domain{..} -> 
                      b <> "," <> encode _domainName <> ":" <> encode a