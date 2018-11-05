module Hson
    ( hson
    , decode
    ) where

import Parser
import Text.Megaparsec
import Text.Megaparsec.Error

hson :: IO ()
hson = interact decode

decode :: String -> String
decode str = case parse jJSON "(undfined)" str of
               Left bundle -> errorBundlePretty bundle
               Right value -> show value

