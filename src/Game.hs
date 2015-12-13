module Game
    (Game(..)
    ,newGame
    ,update
    ,addEvent
    ,Coin(..)
    ,addCoin
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

import qualified Data.Map.Strict as Map
import qualified Data.Random.Normal as RN
import Data.Maybe (mapMaybe,catMaybes)
import Control.Monad.State

import Plane
import Util
import World
import View


data Object = Object
    {objectPosition     :: !Point

    -- |time -> Maybe Picture
    -- destroy the object when false is returned
    ,drawObject         :: Picture
    ,updateObject       :: Float -> State Game (Maybe Object)
}

{-
staticObject point pic = o
    where
        o = Object{objectPosition=point, drawObject=pic, updateObject = \_ -> return $ Just o}
-}


updateMultipleObjects :: Float -> State Game ()
updateMultipleObjects time = do
    -- get all objects
    objs <- gets objects
    -- reset list (allows insertion in update)
    modify $ \g -> g{objects=[]}
    -- run computation
    newobjs <- forM objs $ \o -> updateObject o time
    -- add all objects again
    modify $ \g -> g{objects=objects g++catMaybes newobjs}
        

runEvents :: Float -> State Game ()
runEvents time = do
        evts    <- gets events
        (x,_)   <- gets playerPos
        let (consumed, rest) = x `Map.split` evts
        modify $ \g -> g{events=rest}
        restGame    <- get
        put $ Map.foldr (foldr (\f -> (.)(f time) ) id) restGame consumed


data Game = Game
    {view           :: !View
    ,movement       :: !PlaneMovement
    ,player         :: !Plane
    ,otherPlanes    :: [Plane]
    ,events         :: Map.Map Float [(Float -> Game -> Game)]
    ,score          :: !Int
    ,wind           :: !Vector
    ,gust           :: !Vector
    ,gustDev        :: !Float
    ,lastTime       :: !Float
    ,objects        :: ![Object]
    }
    | GameOver
    {view           :: !View
    }


newGame :: Int -> Int -> IO Game
newGame w h = return $ Game
    {view       = View{pos=(0,0),width=fromIntegral w,height=fromIntegral h}
    ,movement   = Hold
    ,player     = Plane {position=(0,10),velocity=(20,0),angle=0,movement=Hold,planeType=1}
    ,otherPlanes =
        [Plane {position=(0,5),velocity=(0.8,0),angle=0,movement=Hold,planeType=0}
        ]
    ,events     = Map.empty
    ,score      = 0
    ,gust       = (0,0)
    ,gustDev    = 0
    ,wind       = (0,0)
    ,lastTime   = 0
    ,objects    = []
    }


addObject :: Object -> State Game ()
addObject o =
    modify $ \g -> g{objects = o:objects g}

playerPos Game{..} = position player


randomVec dev = do
    x <- RN.normalIO' (0, dev)
    y <- RN.normalIO' (0, dev)
    return $ (x,y)


data Coin = Bronze | Silver | Gold
coinToScore Bronze  = 5
coinToScore Silver  = 10
coinToScore Gold    = 30




bronzeCoin = png "images/coin_bronze.png"

addCoin :: Coin -> Point -> State Game ()
addCoin coin point = addObject obj
    where
        got game = game { score = score game + coinToScore coin }
        obj = Object{objectPosition=point, drawObject=bronzeCoin,
            updateObject = update }
        update time = (do
            pos <- gets playerPos
            return $ if (pos `distV` point < 10) then Nothing else Just obj)
            


addEvent :: Float -> (Float -> Game -> Game) -> State Game ()
addEvent marker func = do
        oldMap <- gets events
        let newMap = Map.insertWith (++) marker [func] oldMap
        modify $ \g -> g{events=newMap}


oneShotEvent :: Float -> (Game -> Game) -> State Game ()
oneShotEvent marker f = addEvent marker (const f)


{-
areaEffect begin end f = addEvent begin event
    where
        event time game = reinsert $ f time game
        reinsert g = let (x,_) = playerPos g in if x<=end then addEvent x event g else g
-}


update :: Float -> Game -> IO Game
update time game@GameOver{} = return game

update time game@Game{..} = do
    newGust <- randomVec gustDev
    if collides World newPlayer then
        return GameOver{view=View{pos=(0,0),width=width view,height=height view}}
    else return $ execState (updateGameState  time) $ game
        { player=newPlayer
        , otherPlanes = newPlane `map` otherPlanes
        , view=view {pos = position newPlayer}
        --, gust = gust `addV` (time `mulSV` gustDev`)
        , lastTime = time
        }
    where
        newPlane = updatePlane time currentWind 
        newPlayer = newPlane player
        currentWind = wind
        timedif = timedif

updateGameState time = do
    runEvents time
    updateMultipleObjects time
    



--runEventBlock
