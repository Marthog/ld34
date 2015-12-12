module Util
(addV,negV,subV
) where

import Graphics.Gloss.Data.Vector

addV :: Vector -> Vector -> Vector
addV (x0,y0) (x1,y1) = (x0+x1,y0+y1)

negV :: Vector -> Vector
negV = mulSV (-1)


subV :: Vector -> Vector -> Vector
subV v0 v1 = addV v0 (negV v1)

