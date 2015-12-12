module World
    (World(..)
    ,newWorld
    ,collides
) where

import Graphics.Gloss
import Plane


data World = World


newWorld :: IO World
newWorld = return World


drawWorld :: Point -> Picture
drawWorld (x,y) = blank


collides world plane =
        y<0
    where
        (x,y) = position plane

