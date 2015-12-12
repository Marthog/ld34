module Plane
    (Plane(..)
    ,drawPlane
    ,up,down
    ,PlaneMovement(..)
    ,updatePlane
    ,airLift
    ,airDrag
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


planeThrust = 30000
wingArea    = 10

-- lift = 1/2 * airDensity * square(airspeed) {- relative -}  * wingArea * liftCoefficient

square x = x*x

-- https://en.wikipedia.org/wiki/Density_of_air#Altitude
airPressure :: Float -> Float
airPressure height = 1000*((1-0.0065*height/288) ** (9.81 * 0.03 / 8.315 / 0.0065))

airDensity :: Float -> Float -> Float
airDensity temperature height= airPressure height * 0.03 / (8.315 * temperature)

-- lift temperatore height wing wingArea = 1/2 * airDensity temperature height * square airSpeed * wingArea * liftCoefficient height
airLift height angle airSpeed wingArea = 0.5 * airDensity 288 height * square airSpeed * liftCoefficient angle * wingArea
airDrag height angle airSpeed wingArea = 0.5 * airDensity 288 height * square airSpeed * 0.4 * 10

-- approximate https://en.wikipedia.org/wiki/Lift_coefficient
liftCoefficient angle = if angle>(-5) && angle<15
    then angle*0.11+0.5
    else if angle>(-5) && angle<25
    then (15-angle)*0.11+0.5
    else 0

planeMass = 30*1000

updatePlane :: Float -> Plane -> Plane
updatePlane time plane@Plane{..} = 
    plane
        {angle      = newAngle
        ,position   = position `addV` (time `mulSV` velocity)
        -- first just a stupid calculation
        ,velocity   = relSpeed `addV` (time `mulSV` acceleration)
    }
    where
        (_,height)  = position
        newAngle    = angle + movementToFactor movement * time * 45
        angleRad    = newAngle * pi/180
        -- acceleration    = (planeThrust*time) `mulSV` unitVectorAtAngle angleRad
        gravity     = 9.81 `mulSV` (0,-1)
        -- relSpeed    = (drag `mulSV` velocity) `addV` acceleration `addV` gravity
        relSpeed    = velocity
        lift        = airLift height ((angleVV (unitVectorAtAngle angleRad) relSpeed)*180/pi) (magV relSpeed) 10
        drag        = airDrag height ((angleVV (unitVectorAtAngle angleRad) relSpeed)*180/pi) (magV relSpeed) 10
        liftVec     = lift `mulSV` unitVectorAtAngle (angleRad+pi/2)
        dragVec     = drag `mulSV` unitVectorAtAngle angleRad
        thrustVec   = planeThrust `mulSV` unitVectorAtAngle angleRad
        acceleration    = (1/planeMass) `mulSV` ( thrustVec `addV` liftVec
                        `subV` dragVec) `addV` gravity

planes = scale 0.1 0.1 `map` [png $ "images/plane" ++ show i ++ ".png" | i <- [0..]]

