module World
    (World(..)
    ,newWorld
    ,collides
    ,drawWorld
) where

import Graphics.Gloss
import Graphics.Gloss.Game
import Plane
import View


data World = World


newWorld :: IO World
newWorld = return World


drawWorld :: View -> Picture
--drawWorld view = repeatX 100 (0) view $ translate 0 (-10000) $ rectangleSolid 100 20000
drawWorld view = pictures[sky, horizon, ground]
    where
        ground = repeatX 800 view $ translate 400 0 $ png "images/background_grass.png"
        horizon = repeatX 8000 view $ scale 10 10 $ translate 400 300 $ png "images/background.png"
        sky = repeatY 6000 view $ repeatX 8000 view $ scale 10 10 $ translate 400 300 $ png "images/background_sky.png"


collides world plane = y<0
    where
        (x,y) = position plane

