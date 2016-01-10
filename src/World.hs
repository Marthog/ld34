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
        horizon = repeatX 2000 view $ scale 5 5 $ translate 400 300 $ png "images/background.png"
        sky = repeatY 6000 view $ repeatX 8000 view $ scale 10 10 $ translate 400 300 $ png "images/background_sky.png"


collides world plane = y<0
    where
        (x,y) = position plane



