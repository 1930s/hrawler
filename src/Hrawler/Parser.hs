{-# LANGUAGE OverloadedStrings #-}

module Hrawler.Parser where

import qualified Data.ByteString.Lazy.Char8 as Char8
import Text.HTML.TagSoup

import Hrawler.Url

data Selector = Selector String | SelectorAttr String String deriving (Show);
data ParserConfig = Container Selector ParserConfig | Get Selector deriving (Show);

sel :: Selector -> String
sel (Selector selector) = selector
sel (SelectorAttr selector _) = selector

getTag :: Selector -> Tag String
getTag selector = toTagRep $ sel selector

parser :: ParserConfig -> [Tag String] -> [Maybe String]
parser (Container sel config) input = parser config input'
      where
        input' = concat $ get_block tag input
        tag = getTag sel

parser (Get (Selector tag)) input = map(extract . filter (isTagText)) matches
      where
        matches = partitions (~== tag) input
        extract :: [Tag String] -> Maybe String
        extract [] = Nothing
        extract l = Just(fromTagText $ head l)

parser (Get (SelectorAttr tag attr)) input = map (extract) matches
      where
        matches = partitions (~== tag) input
        extract :: [Tag String] -> Maybe String
        extract [] = Nothing
        extract l = Just(fromAttrib attr $ head l)

extractBlock :: Int -> String -> Bool -> [Tag String] -> [Tag String]
extractBlock _ _ _ [] = []
extractBlock acc tag terminate (x:xs)
      | terminate == True = []
      | otherwise = x : (extractBlock acc' tag term' xs)
      where acc' = if isTagOpenName tag x then
                      acc + 1
                   else if isTagCloseName tag x then
                      acc - 1
                   else
                      acc
            term' = acc' == 0

get_block :: Tag String -> [Tag String] -> [[Tag String]]
get_block _ [] = []
get_block (TagOpen tag attrs) input = match : (get_block (TagOpen tag attrs) $ drop (length match) input')
        where
              input' = dropWhile (~/= (TagOpen tag attrs)) input
              match = extractBlock 0 tag False input'
