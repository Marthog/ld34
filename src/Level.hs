module Level(
    loadLevel
) where

import Control.Monad.Random
import Control.Monad.State
import Control.Monad

import Game
import Graphics.Gloss.Data.Point


gold = Coin Gold
silver = Coin Silver
bronze = Coin Bronze


m a b c = addItem c (a,b)


level0 =
    [m 70 20 gold
    ,m 50 10 bronze
    ,m 50 20 silver
    ,m 60 10 silver
    ,m 70 10 $ Target 2
    ]

level1 =
    [m 30 5 Explosive
    ,m 35 7 Explosive
    ,m 40 10 Explosive
    ,m 45 14 Explosive
    ,m 55 18 Explosive
    ,m 55 34 Explosive
    ,m 60 35 Explosive
    ,m 70 30 Explosive
    ,m 80 40 Explosive
    ,m 80 2 $ Target 1

    ,m 15 10 bronze
    ,m 23 11 bronze
    ,m 32 13 bronze
    ,m 40 16 bronze
    ,m 48 19 bronze
    ,m 54 21 bronze
    ,m 66 20 bronze
    ,m 78 6 bronze
    ,m 60 15 silver
    ,m 70 10 bronze
    ,m 80 10 gold
    ]

level2 = 
    [m 50 10 gold
    ,m 60 10 Explosive
    ,m 70 10 $ Target 2
    ]


-- |generate steps up (or down) from p0 to p1
steps (x0,y0) (x1,y1) items = do
    dist <- liftRand $ randomR (6,12)
    let ls = [(x0+x*dist, s*x*dist+y0) | x <- [0 .. (x1-x0)/dist]]
    forM ls $ \(x,y) -> do 
        xr <- liftRand $ randomR (-2, 2)
        yr <- liftRand $ randomR (-2, 2)
        itm <- uniform items
        return $ m (x+xr) (y+yr) itm

    where
        s = (y1-y0)/(x1-x0)



data IntervalType
-- |slow increase
    = Inc Float
-- |slow decrease
    | Dec Float
-- |slow increase and rapid fall
    | IncFall Float
    | RandomInterval
    | End


data IntervalRestriction
-- |give score 
    = Bonus
    | BombsBelow
    | BombsAbove

data Choice = Up | Down

-- makeInterval :: Point -> Rand _ _
makeInterval p@(x,y) = do
    next <- uniform [Up, Down]
    end <- liftRand (case next of
        Up -> randomR (min y 120, 120)
        Down -> randomR (20, max y 20)
        )
    let length = 3 * abs(end-y)
    bonus <- uniform [False,True,True]
    above <- uniform [False,True]
    below <- uniform [False,True]


    a0 <- if bonus then steps (x,y) (x+length,end) (gold:medals++medals)
        else return []
    a1 <- if below then steps (x,y-10) (x+length,end-10) [Explosive] 
        else return []
    a2 <- if above then steps (x,y+10) (x+length,end+10) [Explosive]
        else return []
    
    if not bonus && not above && not below
        then makeInterval p
        else return $ (a0++a1++a2, x+length, end)
    where
        medals = [bronze, bronze,bronze,silver]

randomFloat low high = liftRand $ randomR (low, high)

nIntervals 0 p = return ([],p)
nIntervals n p = do
    (ls, x, y) <- makeInterval p
    (rest, p2) <- nIntervals (n-1) (x,y)
    return $ (ls ++ rest,p2)


-- generateFuel :: Float -> Float -> _
generateFuel begin end = do
    if dist > 100 then do
        a <- generateFuel begin middle
        b <- generateFuel middle end
        return $ a++b
    else
        replicateM counter $ do
            x <- randomFloat begin end
            y <- randomFloat 10 100
            size <- randomFloat 4 60
            return $ m x y $ Fuel size
    where
        counter = round (dist/10) :: Int
        dist = end-begin
        middle = begin+dist/2

generateLevel i = (flip evalRand) rand $ do
        -- steps (30,10) (100,50) [bronze, silver, gold]
        (itms, (xe,ye)) <- nIntervals 4 (20,10)
        fuel <- generateFuel 0 xe
        return $ fuel ++ itms ++ [m (xe+10) ye $ Target 3]
    where
        rand = mkStdGen i


levels = 
    [level0
    ,level1
    ,level2
    ]

loadLevel s | s<3       = sequence_ (levels !! s) :: State Game ()
            | otherwise = sequence_ $ generateLevel s
