{-# Language TypeApplications #-}
{-# Language RecordWildCards #-}
module AdventOfCode where
import System.Environment (getArgs)
import Options.Applicative
import Data.Char

import AdventOfCode.Days
import AdventOfCode.Types

data Config = Config
  { day   :: AdventOfCodeDay
  , part  :: AdventOfCodePart
  , input :: FilePath
  }

lowercaseReader :: Read a => ReadM a
lowercaseReader = eitherReader $ \s ->
  let s' = case s of
             (l:ls) -> (toUpper l) : ls
             ls -> ls
  in case reads s' of
       [(r,"")] -> pure r
       _ -> Left $ "cannot parse value: `" <> s' <> "`"

config :: Parser Config
config = Config
  <$> option lowercaseReader
      ( long "day"
      <> short 'd'
      <> showDefault
      <> value (maxBound @AdventOfCodeDay)
      <> help "which day to run" )
  <*> option lowercaseReader
      ( long "part"
      <> short 'p'
      <> help "run 'part1' or 'part2'")
  <*> strOption
      ( long "input"
      <> short 'i'
      <> showDefault
      <> value "input.txt"
      <> help "input file" )

getConfig :: IO Config
getConfig = execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
      <> progDesc "run an advent of code part"
      <> header "run advent of code" )

getPuzzle :: Config -> Puzzle IO ()
getPuzzle Config{..} = lookupPuzzle day part

adventOfCodeMain :: IO ()
adventOfCodeMain = do
  cfg <- getConfig
  runPuzzle (getPuzzle cfg) (input cfg)
