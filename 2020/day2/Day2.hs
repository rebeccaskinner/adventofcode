{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Char
import           GHC.Arr
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)

import qualified Data.Text.Lazy     as Text
import qualified Data.Text.Lazy.IO  as Text

parsePart1Row :: Text.Text -> Maybe (Int,Int,Char,Text.Text)
parsePart1Row t = do
  let
    getComponents t =
      case Text.words t of
        [range,letter,password] ->
          Just (range,letter,password)
        _ -> Nothing

    parseRange r =
      case mapM (readMaybe . Text.unpack) (Text.splitOn "-" r) of
        Just [rangeMin,rangeMax] -> Just (rangeMin,rangeMax)
        _                        -> Nothing

    parseLetter l =
      case (Text.unpack . Text.takeWhile (/= ':')) l of
        [c] -> Just c
        _   -> Nothing

  (range,letter,password) <- getComponents t
  (rangeMin, rangeMax) <- parseRange range
  letter' <- parseLetter letter
  pure $ (rangeMin,rangeMax,letter',password)


validatePart1Row :: Char -> (Int -> Bool) -> String -> Bool
validatePart1Row refChar pred t = runST $ do
  arr <- thawSTArray $ listArray ('a','z') (repeat 0)
  forM_ t $ \c -> do
    cnt <- readSTArray arr c
    writeSTArray arr c (cnt + 1)
  n <- readSTArray arr refChar
  pure $ pred n

valueInRange :: Ord a => (a,a) -> a -> Bool
valueInRange (valMin, valMax) val =
  (val >= valMin) && (val <= valMax)

computeRowEligible :: Text.Text -> Bool
computeRowEligible txt =
  case parsePart1Row txt of
    Nothing -> False
    Just (minCnt, maxCnt, c, pw) ->
      validatePart1Row c (valueInRange (minCnt,maxCnt)) (Text.unpack pw)

part1 :: FilePath -> IO ()
part1 fname = do
  t <- Text.lines <$> Text.readFile fname
  let r = filter computeRowEligible t
  putStrLn "Valid Passwords"
  mapM_ print r
  putStrLn $ "total: " <> show (length r)

validatePart2Row :: (Int,Int) -> Char -> Text.Text -> Bool
validatePart2Row (idx,idx') c t =
  let
    i  = Text.index t (fromIntegral $ pred idx)
    i' = Text.index t (fromIntegral $ pred idx')
  in (c == i) `xor` (c == i')

computeRowEligible' :: Text.Text -> Bool
computeRowEligible' t =
  case parsePart1Row t of
    Just (idx,idx',c,pw) -> validatePart2Row (idx,idx') c pw
    _                    -> False

part2 :: FilePath -> IO ()
part2 fname = do
  t <- Text.lines <$> Text.readFile fname
  let r = filter computeRowEligible' t
  putStrLn "Valid Passwords"
  mapM_ print r
  putStrLn $ "total: " <> show (length r)

main :: IO ()
main = do
  (part:fname:_) <- getArgs
  case part of
    "part1" -> part1 fname
    "part2" -> part2 fname
    badCmd  -> putStrLn $ "Unknown command: " <> badCmd
