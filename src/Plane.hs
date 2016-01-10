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

module Plane
    (Plane(..)
    ,drawPlane
    ,up,down
    ,PlaneMovement(..)
    ,updatePlane
) where

import Control.Monad.State

import Graphics.Gloss.Data.Vector
import Graphics.Gloss
import Graphics.Gloss.Game
import Util


data PlaneMovement = MoveUp | MoveDown | Hold
    deriving (Show, Eq)


up plane = plane { movement = upF $ movement plane }
down plane = plane { movement = downF $ movement plane }

upF MoveDown = Hold
upF _        = MoveUp

downF MoveUp = Hold
downF _      = MoveDown

movementToFactor m = case m of
    MoveUp      -> 1
    MoveDown    -> -1
    Hold        -> 0


data Plane = Plane
    {position           :: !Point
    -- |current velocity of the plane
    ,velocity           :: !Vector
    -- |the angle that the plane points and accelerates into
    ,angle              :: !Float
    ,movement           :: !PlaneMovement
    ,planeType          :: !Int
    ,fuel               :: !Float
    ,willDie            :: !Bool
    }


drawPlane :: Plane -> Picture
drawPlane Plane{..} =
        translate x y $ rotate (-angle) (planes !! planeType)
    where (x,y) = position



{-
 - These formulas are complete crap
 -
 - they are physically incorrect but they are simple
 - and they are a lot of fun
-}

planeThrust = 13000
mass = 1000
drag        = 0.985
planeLift   = 1.0

updatePlane :: Float -> Vector -> Plane -> Plane
updatePlane time wind plane@Plane{..} = 
    plane
        {angle      = newAngle
        ,position   = position `addV` (time `mulSV` airSpeed)
        -- first just a stupid calculation
        ,velocity   = drag `mulSV` (velocity `addV` acceleration `addV` gravity `addV` lift)
        ,fuel       = max 0 $ fuel-time
    }
    where
        thrust      = if willDie || fuel<=0 then 0 else planeThrust 
        (_,height)  = position
        newAngle    = angle + if willDie then 0 else movementToFactor movement * time * 45
        airSpeed    = wind `addV` velocity
        angleRad    = newAngle * pi/180
        currentMass = mass+(fuel*10)
        acceleration    = (thrust*time/currentMass) `mulSV` unitVectorAtAngle angleRad
        gravity     = 10*time `mulSV` (0,-1)
        lift        = ((planeLift*((1,0) `dotV` acceleration))*(clamp 0 ((150-height)**2/50) 1)) `mulSV` (0,1)


{-
updatePlaneRun :: Float -> Vector -> State Plane ()
updatePlaneRun time wind = do
    angle       <- gets angle
    moveFactor  <- movementToFactor `fmap` gets movement
    let newAngle    = angle +  moveFactor * time * 45
    modify $ \p -> p{angle=newAngle}
-}
 

planes = scale 0.1 0.1 `map` [png $ "images/plane" ++ show i ++ ".png" | i <- [0..]]

clamp a b c = max a (min b c)
