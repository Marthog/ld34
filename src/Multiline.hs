{-
    This file is part of ChaosFlight.


    ChaosFlight is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ChaosFlight is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ChaosFlight.  If not, see <http://www.gnu.org/licenses/>.
-}

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

