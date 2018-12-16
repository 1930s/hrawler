{-# LANGUAGE OverloadedStrings #-}

module Hrawler.Fetcher where

import Network.HTTP.Conduit

import Data.ByteString.Lazy.Char8 as Char8

import Hrawler.Url

fetch :: String -> IO ByteString
fetch url = simpleHttp url
