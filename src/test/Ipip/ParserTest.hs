{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ipip.ParserTest where

import Data.List            (tail, last)
import Text.Email.Validate  (unsafeEmailAddress, EmailAddress)

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Text.HTML.TagSoup    (Tag(..))

import qualified Data.Text        as T
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range

import Bot.Types
import Ipip.Parsers
import Paths_mtl_classy_prism    (getDataFileName)
import Utils                (mkBigFiveResult)

test_IpipParser :: TestTree
test_IpipParser = testGroup "Ipip data Parsers"
  [ testGroup "parseFacet"
    [ testProperty "textbody ended with space or w/o single space" test_parseFacet_tagtext_ended_with_without_space
    , testProperty "handle different `Tag` data constructor" test_parseFacet_any_tag_constructor
    , testGroup "parse invalid facet textbody"
      [ testProperty "when facet name contains `.`" test_parseFacet_name_contains_dot
      , testProperty "when facet has no name" test_parseFacet_no_name
      , testProperty "textbody w/o `.`" test_parseFacet_tagtext_without_dot
      , testProperty "textbody w/o integer (i.e. score)" test_parseFacet_tagtext_without_score
      , testProperty "textbody ended with multiple spaces" test_parseFacet_tagtext_ended_with_spaces
      , testProperty "textbody ended with neither integer nor space" test_parseFacet_tagtext_ended_with_neither_integer_nor_space ]
    ]
  , testProperty "parseDomain from a list of `Tag` data constructors" test_parseDomain
  , testProperty "parseDomains from any number of groups of tags" test_parseDomains
  , testProperty "parseBigFiveResult" test_parseBigFiveResult
  , testGroup "parseIpipFile"
    [ testProperty "parseIpipFile from resources/test/ipip/big5result.html" test_parseIpipFile_big5result
    , testProperty "parseIpipFile from resources/test/ipip/big5result-slim.html" test_parseIpipFile_big5result_slim ]
  ]

-- | test that if there is only 1 space after the text such as
-- @<p>OPENNESS TO EXPERIENCE...83 </p>@
-- or no space such as @<p>OPENNESS TO EXPERIENCE...83</p>@
-- it can parse the text successfully
test_parseFacet_tagtext_ended_with_without_space :: Property
test_parseFacet_tagtext_ended_with_without_space = property do
  _facetName <- forAll genFacetName
  dots <- forAll genDots
  _facetScore <- forAll genFacetScore
  let expected = Right Facet{..}
      textEndWithNoSpace = _facetName <> dots <> show _facetScore
      facetResults = parseFacet . TagText . toS . ($ textEndWithNoSpace) <$> [identity, (<> " ")]
  assert $ all (== expected) facetResults

-- | 'Tag Text' has several data constructors, 
-- facet information is only available from 'TagText'.
-- Test that it only parses the facet information from 'TagText'
-- and ignor the other constructors. 
test_parseFacet_any_tag_constructor :: Property
test_parseFacet_any_tag_constructor = property do
  tag <- forAll genTag
  let facetResult = parseFacet tag
  case tag of
    TagText _ -> assert $ isRight facetResult
    _ -> facetResult === Left "Facet information only comes from TagText"

-- | Test that it handles the condition accordingly when 
-- the name contains dot
test_parseFacet_name_contains_dot :: Property
test_parseFacet_name_contains_dot = property do 
  name <- forAll genNameHavingDot
  dots <- forAll genDots
  score <- forAll genFacetScore
  -- if dot exists at the end, it will be treated in the same way as the subsequent dots
  let rightExpected = last (toS name) == '.' 
      facetResult = parseFacet . TagText . toS $ name <> dots <> show score
  isRight facetResult === rightExpected
  where 
    genNameHavingDot = genFacetName >>= flip insertChar '.'

-- | It should fail
test_parseFacet_no_name :: Property
test_parseFacet_no_name = property do
  dots <- forAll genDots
  score <- forAll genFacetScore
  let facetResult = parseFacet . TagText . toS $ dots <> show score
  assert $ isLeft facetResult
    
-- | It should fail
test_parseFacet_tagtext_without_dot :: Property
test_parseFacet_tagtext_without_dot = property do
  name <- forAll genFacetName
  score <- forAll genFacetScore
  assert (isLeft . parseFacet . TagText . toS $ name <> show score)

-- | It should fail
test_parseFacet_tagtext_without_score :: Property
test_parseFacet_tagtext_without_score = property do
  name <- forAll genFacetName
  dots <- forAll genDots
  assert (isLeft . parseFacet . TagText $ name <> dots)

-- | It should fail since the text ends with > 1 space
test_parseFacet_tagtext_ended_with_spaces :: Property
test_parseFacet_tagtext_ended_with_spaces = property do
  name <- forAll genFacetName
  dots <- forAll genDots
  score <- forAll genFacetScore
  spaces <- (" " <>) <$> forAll genSpaces
  assert (isLeft . parseFacet . TagText . toS $ name <> dots <> show score <> spaces)

-- | It should fail given the text neither ends with a space nor integer
test_parseFacet_tagtext_ended_with_neither_integer_nor_space :: Property
test_parseFacet_tagtext_ended_with_neither_integer_nor_space = property do
  name <- forAll genFacetName
  dots <- forAll genDots
  score <- forAll genFacetScore
  -- genFacetName generates at least 2 char length text, so it should fail
  ending <- forAll genFacetName 
  assert (isLeft . parseFacet . TagText . toS $ name <> dots <> show score <> ending)

-- | Test 'parseDomain'
-- 'parseFacet' has been tested in the previous tests, 
-- it can be used to produce the expected result for checking
test_parseDomain :: Property
test_parseDomain = property do
  tags <- forAll tagsGen
  let domainResult = parseDomain tags
  -- there may be other tags not parsable to facet
  -- therefore pick facet tags for 'parseFacet' s.t. 
  -- a correct expected result can be produced
  case filter pickFacetTag tags of
    [] -> domainResult === Left "No facet information to build a domain score"
    xs -> checkResult domainResult (traverse parseFacet xs)
  where
    tagsGen = Gen.list (Range.linear 100 1600) genTag
    pickFacetTag (TagText _) = True
    pickFacetTag _ = False
    checkResult domainResult expectedFacets = assert $ and
      [ isRight domainResult
      , fmap (\Domain{..} -> Just $ Facet _domainName _domainScore) domainResult == fmap head expectedFacets
      , fmap _domainFacets domainResult == fmap tail expectedFacets ]

-- | Test 'parseDomains'
test_parseDomains :: Property
test_parseDomains = property do
  groups <- forAll genGroups
  -- to mock the html file content
  -- intercalate the groups with other generated tags 
  -- to see how 'parseDomains' handle them
  groupsIntercalatedByTags <- forAll $ genGroupsIntercalateByTags groups
  -- not all groups contain facet tag
  -- therefore pick those groups having facet tag for 'parseDomain' s.t. 
  -- a correct expected result can be produced
  let expected = traverse parseDomain $ filter hasFacet groups
      domainsResult = parseDomains groupsIntercalatedByTags
  Right domainsResult === expected

-- | Test 'parseBigFiveResult'
-- It is similar to 'test_parseDomains' except
-- it also check username, useremail which do not originate from the tags
-- and if the function handles the condition when no. of domains is not 5
test_parseBigFiveResult :: Property
test_parseBigFiveResult = property do
  groups <- forAll genGroups
  groupsIntercalatedByTags <- forAll (genGroupsIntercalateByTags groups)
  let _userName = "Colossal Titan" 
      _userEmail = unsafeEmailAddress "colossal.titan" "aot.com"
      _userIpipFile = "ignored-by-parseBigFiveResult"
      expected =  case length $ filter hasFacet groups of
                    5 -> Right . BigFiveResult _userName _userEmail . parseDomains $ concat groups
                    _ -> Left (IpipParseError "5 domains expected from the html data")
  bigFiveResult <- parseBigFiveResult' UserInfo{..} groupsIntercalatedByTags
  bigFiveResult === expected
  where 
    parseBigFiveResult' :: UserInfo -> [Tag Text] -> PropertyT IO (Either ParseError BigFiveResult)
    parseBigFiveResult' userInfo = runExceptT . parseBigFiveResult userInfo

-- | Test 'parseIpipFile' with sample file
test_parseIpipFile_big5result :: Property
test_parseIpipFile_big5result = 
  test_parseIpipFile 
    "Colossal Titan" 
    (unsafeEmailAddress "colossal.titan" "aot.com")
    "test/ipip/big5result.html" 

-- | Test 'parseIpipFile' with a similar file where irrelevant tags are removed
test_parseIpipFile_big5result_slim :: Property
test_parseIpipFile_big5result_slim = 
  test_parseIpipFile 
    "Lambda" 
    (unsafeEmailAddress "lambda" "category.org")
    "test/ipip/big5result-slim.html" 

-- | A common function for parseIpipFile tests
test_parseIpipFile :: Text -> EmailAddress -> FilePath -> Property
test_parseIpipFile _userName _userEmail fileLocation = property do
  _userIpipFile <- liftIO . fmap toS . getDataFileName $ fileLocation
  bigFiveResult <- parseIpipFile' UserInfo{..}
  let screenCapped =
        [ ("EXTRAVERSION",98) :| [("Friendliness",95), ("Gregariousness",91), ("Assertiveness",74), ("Activity Level",78), ("Excitement-Seeking",93), ("Cheerfulness",88)]
        , ("AGREEABLENESS",54) :| [("Trust",80), ("Morality",55), ("Altruism",77), ("Cooperation",52), ("Modesty",25), ("Sympathy",94)]
        , ("CONSCIENTIOUSNESS",88) :| [("Self-Efficacy",96), ("Orderliness",92), ("Dutifulness",61), ("Achievement-Striving",73), ("Self-Discipline",78), ("Cautiousness",55)]
        , ("NEUROTICISM",7) :| [("Anxiety",30), ("Anger",23), ("Depression",5), ("Self-Consciousness",8), ("Immoderation",50), ("Vulnerability",21)]
        , ("OPENNESS TO EXPERIENCE",83) :| [("Imagination",55), ("Artistic Interests",81), ("Emotionality",44), ("Adventurousness",89), ("Intellect",70), ("Liberalism",78)] ]
      expected = Right $ mkBigFiveResult _userName _userEmail screenCapped                                
  bigFiveResult === expected
  where
    parseIpipFile' :: UserInfo -> PropertyT IO (Either ParseError BigFiveResult)
    parseIpipFile' = runExceptT . parseIpipFile

-- | Concatenates groups of '[Tag Text]' by intercalating 
-- randomly-generated '[Tag Text]' except @<div class="graph-txt">@
genGroupsIntercalateByTags :: [[Tag Text]] -> Gen [Tag Text]
genGroupsIntercalateByTags = foldl accumulate $ return []
  where 
    accumulate genAcc tagsOfGroup = 
      do
        accumulatedTags <- genAcc
        intercalatedTags <- genIntercalatedTags
        return $ accumulatedTags <> intercalatedTags <> tagsOfGroup 
    genIntercalatedTags = sequence . replicate 5 $ genTagExcludedBy [graphTxtOpenTag]

-- | Generates groups of '[Tag Text]',
-- each group is '[Tag Text]' enclosed by @<div class="graph-txt">@ and @</div>@
genGroups :: Gen [[Tag Text]]
genGroups = Gen.list (Range.linear 0 10) genGroup
  where
    genGroup = do 
      tags <- sequence $ replicate 10 genTag'
      return (graphTxtOpenTag : tags <> [divCloseTag])
    -- @<div class="graph-txt">@ and @</div>@ are the group's boundary
    -- therefore any of them should not exist within the group as well,
    -- o.w. it will parse incorrectly
    genTag' = genTagExcludedBy [graphTxtOpenTag, divCloseTag]

genFacetName :: Gen Text
genFacetName = toS <$> Gen.list (Range.linear 2 20) (Gen.element availableChars)
  where 
    availableChars = ['a'..'z'] <> ['A'..'Z'] <> "/ -"

-- | Generates dots of random length
genDots :: Gen Text
genDots = genRepeatedCharText '.'

-- | Generates spaces of random length
genSpaces :: Gen Text
genSpaces = genRepeatedCharText ' '

genRepeatedCharText :: Char -> Gen Text
genRepeatedCharText = fmap toS . Gen.list (Range.linear 1 20) . return

genFacetScore :: Gen Int
genFacetScore = Gen.int (Range.linear 0 100)

-- | Random generator of all possible 'Tag Text' except the excluded tags
genTagExcludedBy :: [Tag Text] -> Gen (Tag Text)
genTagExcludedBy excludedTags = genTag >>= \tag -> 
    if tag `elem` excludedTags then genTagExcludedBy excludedTags else return tag

-- | Random generator of all possible 'Tag Text' data constructors.
-- To simplify the test and checking, 'genFacetTag' always 
-- generates 'TagText' parsable value for facet information
genTag :: Gen (Tag Text)
genTag = Gen.choice 
  [ genOpenTag
  , genCloseTag
  , genFacetTag
  , genCommentTag
  , genWarningTag
  , genPositionTag ]

genOpenTag :: Gen (Tag Text)
genOpenTag = TagOpen <$> genDummyText <*> attributesGen
  where
    attributesGen = Gen.list (Range.linear 0 3) attributeGen
    attributeGen = (,) <$> genDummyText <*> genDummyText

genCloseTag :: Gen (Tag Text)
genCloseTag = TagClose <$> genDummyText

-- | Random generator of 'Tag Text' containing parsable facet information
genFacetTag :: Gen (Tag Text)
genFacetTag = TagText <$> validFacetContentGen
  where
    validFacetContentGen = 
      do 
        name <- genFacetName
        dots <- genDots
        score <- genFacetScore      
        return $ name <> dots <> show score

genCommentTag :: Gen (Tag Text)
genCommentTag = TagComment <$> genDummyText

genWarningTag :: Gen (Tag Text)
genWarningTag = TagWarning <$> genDummyText

genPositionTag :: Gen (Tag Text)
genPositionTag = TagPosition <$> genInt <*> genInt
  where
    genInt = Gen.int (Range.linear 1 1000)

-- | Reuses `genFacetName` which can be used to generate a dummy text for a tag as well
genDummyText :: Gen Text
genDummyText = genFacetName

-- | Represents @<div class="graph-txt">@ 
-- This tag indicates the start of the group of tags from which 
-- all facet information of a domain is pared
graphTxtOpenTag :: Tag Text
graphTxtOpenTag = TagOpen "div" [("class","graph-txt")]

-- | Represents @</div>@ 
-- This tag indicates the end of the group of tags from which 
-- all facet information of a domain is pared
divCloseTag :: Tag Text
divCloseTag = TagClose "div"

hasFacet :: [Tag Text] -> Bool
hasFacet = any isFacet

isFacet :: Tag Text -> Bool
isFacet (TagText _) = True
isFacet _ = False

-- | Put the given char at a random position of the given text
insertChar :: Text -> Char -> Gen Text
insertChar s c = 
  Gen.int (Range.linear 0 $ T.length s - 1) <&> \i ->
    let (lead, tail') = T.splitAt i s
    in  lead <> toS case toS tail' of
                      _ : remains -> c : remains
                      _           -> [c]