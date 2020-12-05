module AdventOfCode.Types where
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText

import qualified Data.ByteString.Char8 as BSC

newtype Puzzle m r =
  Puzzle
  { runPuzzle :: FilePath -> m r
  }

data PuzzleDay m = PuzzleDay
  { puzzleDay1 :: Puzzle m ()
  , puzzleDay2 :: Puzzle m ()
  }

withTextInput :: MonadIO m => (Text.Text -> m r) -> Puzzle m r
withTextInput puzzle =
  Puzzle (liftIO . Text.readFile >=> puzzle)

withLazyTextInput :: MonadIO m => (LazyText.Text -> m r) -> Puzzle m r
withLazyTextInput puzzle =
  Puzzle (liftIO . LazyText.readFile >=> puzzle)

withByteStringChar8Input :: MonadIO m => (BSC.ByteString -> m r) -> Puzzle m r
withByteStringChar8Input puzzle =
  Puzzle (liftIO . BSC.readFile >=> puzzle)

withStringInput :: MonadIO m => (String -> m r) -> Puzzle m r
withStringInput puzzle =
  Puzzle (liftIO . readFile >=> puzzle)
