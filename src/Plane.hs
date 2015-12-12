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


updatePlane :: Float -> Plane -> Plane
updatePlane time plane@Plane{..} = 
    plane
        {angle      = newAngle
        ,position   = position `addV` (time `mulSV` velocity)
        -- first just a stupid calculation
        ,velocity   = 10 `mulSV` unitVectorAtAngle angleRad
    }
    where
        newAngle = angle + movementToFactor movement * time * 45
        angleRad = newAngle * pi/180


planes = scale 0.1 0.1 `map` [png $ "images/plane" ++ show i ++ ".png" | i <- [0..]]

