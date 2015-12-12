module Multiline (
    multiline,
    multilineString
) where

import Graphics.Gloss

-- |create multiple lines of text from an array of strings
multiline :: [String] -> Picture
multiline ls = pictures $ zipWith (\a b -> translate 0 (-130*b) (text a)) ls [0..]


-- |create a multiline text from a single string with line breaks '\n'
multilineString :: String -> Picture
multilineString = multiline . lines

