{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AdventOfCode.Days.Day4 where
--  ( part1
--  , part2
--  , day4
--  ) where
import AdventOfCode.Types
import Text.Read
import Data.Maybe
import Data.Char
import Data.Text.Utils
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

type Year = Int

data Passport = Passport
  { birthYear      :: Year
  , issueYear      :: Year
  , expirationYear :: Year
  , height         :: Length
  , hairColor      :: Text.Text
  , eyeColor       :: EyeColor
  , passportId     :: Text.Text
  , countryId      :: Maybe Text.Text
  } deriving (Eq, Show)

data Length = Inches Int | Centimeters Int deriving (Eq,Show)

data EyeColor = Amber | Blue | Brown | Gray | Green | Hazel | Other deriving (Eq,Show)

parseEyeColor :: Text.Text -> Maybe EyeColor
parseEyeColor t =
  case t of
    "amb" -> Just Amber
    "blu" -> Just Blue
    "brn" -> Just Brown
    "gry" -> Just Gray
    "grn" -> Just Green
    "hzl" -> Just Hazel
    "oth" -> Just Other
    _ -> Nothing

readMaybe' :: Read a => Text.Text -> Maybe a
readMaybe' = readMaybe . Text.unpack

parseInRange :: (Read a, Ord a) => (a,a) -> Text.Text -> Maybe a
parseInRange (valMin, valMax) val =
  case readMaybe' val of
    Just val' | (val' >= valMin && val' <= valMax) -> Just val'
    _ -> Nothing

parseHeight :: Text.Text -> Maybe Length
parseHeight t =
  let (val,units) = Text.splitAt (Text.length t - 2) t
  in case units of
       "cm" -> Centimeters <$> parseInRange @Int (150,193) val
       "in" -> Inches <$> parseInRange @Int (59,76) val
       _ -> Nothing

parseHairColor :: Text.Text -> Maybe Text.Text
parseHairColor t =
  let
    hashPrefix = '#' == Text.head t
    hexColor = Text.all isHexDigit (Text.tail t)
  in if hashPrefix && hexColor then Just t else Nothing

validatePassport :: Text.Text -> Maybe Text.Text
validatePassport t =
  if (Text.length t == 9) && (Text.all isDigit t) then Just t else Nothing

mapToPassport' :: Map.Map Text.Text Text.Text -> Maybe Passport
mapToPassport' m = do
  birthYear      <- parseInRange (1920,2002) =<< Map.lookup "byr" m
  issueYear      <- parseInRange (2010,2020) =<< Map.lookup "iyr" m
  expirationYear <- parseInRange (2020,2030) =<< Map.lookup "eyr" m
  height         <- parseHeight              =<< Map.lookup "hgt" m
  hairColor      <- parseHairColor           =<< Map.lookup "hcl" m
  eyeColor       <- parseEyeColor            =<< Map.lookup "ecl" m
  passportId     <- validatePassport         =<< Map.lookup "pid" m
  let countryId = Map.lookup "cid" m
  pure $ Passport{..}

validatePassportFields :: Map.Map Text.Text Text.Text -> Bool
validatePassportFields m = isJust $ do
  _birthYear      <- Map.lookup "byr" m
  _issueYear      <- Map.lookup "iyr" m
  _expirationYear <- Map.lookup "eyr" m
  _height         <- Map.lookup "hgt" m
  _hairColor      <- Map.lookup "hcl" m
  _eyeColor       <- Map.lookup "ecl" m
  _passportId     <- Map.lookup "pid" m
  pure ()


validatePassportEntry :: Text.Text -> Bool
validatePassportEntry =
  validatePassportFields
  . Map.fromList
  . map (splitAtFirst ":")
  . concatMap (Text.words)
  . Text.lines

parsePassport' :: Text.Text -> Maybe Passport
parsePassport' =
  mapToPassport'
  . Map.fromList
  . map (splitAtFirst ":")
  . concatMap (Text.words)
  . Text.lines


getList c = map (map (splitAtFirst ":") .concatMap Text.words . Text.lines) $ breakAtEmptyLines c

part1 :: Puzzle IO ()
part1 = withTextInput $ \contents -> do
  let validPassports = length $ filter validatePassportEntry (breakAtEmptyLines contents)
  putStrLn $ "number of valid passports: " <> show validPassports

part2 :: Puzzle IO ()
part2 = withTextInput $ \contents -> do
  let parsed = catMaybes . map parsePassport' . breakAtEmptyLines $ contents
  putStrLn $ "number of valid passports: " <> show (length parsed)
  pure ()

day4 :: PuzzleDay IO
day4 = PuzzleDay part1 part2
