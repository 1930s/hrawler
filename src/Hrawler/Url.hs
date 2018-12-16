module Hrawler.Url where

normalise_url :: String -> String -> String
normalise_url prefix str = if Prelude.take 6 str == "https:"
    then str
    else prefix ++ str
