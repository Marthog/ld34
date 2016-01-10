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

module Util
(addV,negV,subV
,distV
) where

import Graphics.Gloss.Data.Vector

addV :: Vector -> Vector -> Vector
addV (x0,y0) (x1,y1) = (x0+x1,y0+y1)

negV :: Vector -> Vector
negV = mulSV (-1)


subV :: Vector -> Vector -> Vector
subV v0 v1 = addV v0 (negV v1)


distV :: Vector -> Vector -> Float
distV a b = magV (a `subV` b)
