module Plane
    (Plane(..)
    ,drawPlane
    ,up,down
    ,PlaneMovement(..)
    ,updatePlane
) where


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

planeThrust = 9
drag        = 0.99
planeLift   = 1.0

updatePlane :: Float -> Vector -> Plane -> Plane
updatePlane time wind plane@Plane{..} = 
    plane
        {angle      = newAngle
        ,position   = position `addV` (time `mulSV` airSpeed)
        -- first just a stupid calculation
        ,velocity   = drag `mulSV` velocity `addV` acceleration `addV` gravity `addV` lift
    }
    where
        (_,height)  = position
        newAngle    = angle + movementToFactor movement * time * 45
        airSpeed    = wind `addV` velocity
        angleRad    = newAngle * pi/180
        acceleration    = planeThrust*time `mulSV` unitVectorAtAngle angleRad
        gravity     = 10*time `mulSV` (0,-1)
        lift        = ((planeLift*((1,0) `dotV` acceleration))*(clamp 0 ((150-height)**2/50) 1)) `mulSV` (0,1)



planes = scale 0.1 0.1 `map` [png $ "images/plane" ++ show i ++ ".png" | i <- [0..]]

clamp a b c = max a (min b c)
