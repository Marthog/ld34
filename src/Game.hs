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

module Game
    (Game(..)
    ,newGame
    ,update
    ,addEvent
    ,Coin(..),Item(..)
    ,addItem
    ,drawIngame
    ,startGame
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
    ,victory        :: !Bool
    ,level          :: !Int
    }
    | GameOver
    {view           :: !View
    ,score          :: !Int
    ,victory        :: !Bool
    ,level          :: !Int
    ,first          :: !Bool
    }


newGame :: Int -> Int -> Game
newGame w h = GameOver
    {view   = (View{pos=(0,0),width=fromIntegral w,height=fromIntegral h})
    ,score  = 0
    ,victory=False
    ,level  = 0
    ,first  = True
    }

startGame view lvl = Game
    {view       = view
    ,movement   = Hold
    ,player     = Plane {position=(0,10),velocity=(20,0),angle=0,movement=Hold,planeType=1,fuel=30, willDie=False}
    ,otherPlanes = []
        -- [Plane {position=(0,5),velocity=(0.8,0),angle=0,movement=Hold,planeType=0,fuel=120}]
    ,events     = Map.empty
    ,score      = 0
    ,gust       = (0,0)
    ,gustDev    = 0
    ,wind       = (0,0)
    ,lastTime   = 0
    ,objects    = []
    ,victory    = False
    ,level      = lvl
    }


addObject :: Object -> State Game ()
addObject o =
    modify $ \g -> g{objects = o:objects g}

playerPos Game{..} = position player


randomVec dev = do
    x <- RN.normalIO' (0, dev)
    y <- RN.normalIO' (0, dev)
    return $ (x,y)


data Item = Coin Coin | Fuel !Float | Explosive | Target !Float

data Coin = Bronze | Silver | Gold
coinToScore Bronze  = 5
coinToScore Silver  = 10
coinToScore Gold    = 30

itemRadius (Coin _) = 2
itemRadius (Fuel f) = log (f/2)
    
itemRadius Explosive = 3
itemRadius (Target f) = f

itemVisual (Coin f) = scale (1/20) (1/20) $ png $ "images/" ++ (case f of
    Bronze  ->  "coin_bronze"
    Silver  ->  "coin_silver"
    Gold    ->  "coin_gold")++".png"

itemVisual (Fuel f) = scale s s $ png "images/oil_can.png"
    where
        s = (1/20) * log (f/10)
itemVisual Explosive = scale (1/20) (1/20) $ png "images/explosive.png"
itemVisual (Target f)   = scale s s $ png "images/target.png"
    where
        s = (1/40) * f


itemEffect :: Item -> State Game ()
itemEffect (Coin coin) = do
    s <- gets score
    modify $ \g -> g{score=s+coinToScore coin}

itemEffect (Fuel f) = do
    p <- gets player
    let np = p{fuel = fuel p+f}
    modify $ \g -> g{player=np}

itemEffect Explosive = do
    p <- gets player
    modify $ \g -> g{player = p{willDie=True}}

itemEffect (Target _) = do
    modify $ \g -> g{victory=True}


addItem :: Item -> Point -> State Game()
addItem item point = addObject obj
    where
        obj = Object
            {objectPosition = point
            ,drawObject = itemVisual item
            ,updateObject = update }
        update time = (do
            pos <- gets playerPos
            die <- willDie `fmap` gets player
            if pos `distV` point < itemRadius item && not die then do
                itemEffect item
                return Nothing
            else
                return $ Just obj)


drawIngame :: State Game Picture
drawIngame = do
    objs <- gets objects
    let objectPics = map drawTranslatedObject objs
    player <- gets player
    otherPlanes <- gets otherPlanes
    let planes  = map drawPlane (otherPlanes ++ [player])
    return $ pictures (objectPics++planes)

    where
        drawTranslatedObject Object{..} = (uncurry translate) objectPosition drawObject



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
update time game@Game{victory=True,..} = 
        return GameOver
            {view=View{pos=(0,0),width=width view,height=height view}
            ,score=score
            ,level=level+1
            ,victory=True
            ,first=False}
update time game@Game{..} = do
    newGust <- randomVec gustDev
    if collides World newPlayer then
        return $ GameOver
            {view=View{pos=(0,0),width=width view,height=height view}
            ,victory=False
            ,level=level
            ,score=score
            ,first=False}
    else return $ execState (updateGameState  time) $ game
        { player=newPlayer
        , otherPlanes = newPlane `map` otherPlanes
        , view=view {pos = position newPlayer}
        --, gust = gust `addV` (time `mulSV` gustDev`)
        , Game.lastTime = time
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
