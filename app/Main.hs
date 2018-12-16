{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Printf

import Hrawler.Fetcher
import Hrawler.Parser

import Text.HTML.TagSoup

main :: IO ()
main = do
    let myparser1 = Container (Selector "<ul class=list>")
                      (Container (Selector "<li class=foo>") (Get $ SelectorAttr "<a>" "href"))

    let myparser2 = Get $ Selector "<a>"

    content <- readFile "example.html"

    --src <- fetch "http://learnyouahaskell.com/chapters"
    let tag = TagOpen "ul" [("class","list")]

    print( " ----- " )
    let res1 = parser myparser1 $ parseTags content
    mapM_ ( print ) res1

    print( " ----- " )
    let res2 = parser myparser2 $ parseTags content
    mapM_ ( print ) res2
