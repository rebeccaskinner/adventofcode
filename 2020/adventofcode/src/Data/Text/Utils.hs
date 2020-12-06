{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Utils where
import qualified Data.Text as Text

splitAtFirst :: Text.Text -> Text.Text -> (Text.Text,Text.Text)
splitAtFirst tok t =
  case Text.splitOn tok t of
    [] -> (Text.empty,Text.empty)
    [t'] -> (t',Text.empty)
    (t':ts) -> (t', Text.intercalate tok ts)

breakAtEmptyLines :: Text.Text -> [Text.Text]
breakAtEmptyLines = Text.splitOn "\n\n"
