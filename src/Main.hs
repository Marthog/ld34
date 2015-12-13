
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Graphics.Gloss.Data.Point

import qualified Data.Map as Map

import Plane
import Util
import World
import View


windowWidthI = 800              :: Int
windowHeightI = 600             :: Int



data Game = Game
    {view           :: !View
    ,movement       :: !PlaneMovement
    ,player         :: !Plane
    ,otherPlanes    :: [Plane]
    ,events         :: Map.Map Float [(Game -> Game)]
    ,score          :: !Int
    }
    | GameOver
    {view           :: !View
    }


newGame :: IO Game
newGame = return $ Game
    {view       = View{pos=(0,0),width=fromIntegral windowWidthI,height=fromIntegral windowHeightI}
    ,movement   = Hold
    ,player     = Plane {position=(0,10),velocity=(20,0),angle=0,movement=Hold,planeType=1}
    ,otherPlanes =
        [Plane {position=(0,5),velocity=(0.8,0),angle=0,movement=Hold,planeType=0}
        ]
    ,events     = Map.empty
    ,score      = 0
    }


playerPos Game{..} = position player

input :: Event -> Game -> Game
input (EventResize s) game          = game{view=resize s (view game)}
input (EventKey key state _ _) game@Game{} = keyPress key state game
input event game = game


upButtons = [SpecialKey(KeyUp)]
downButtons = [SpecialKey(KeyDown)]


keyPress key state game = game {
        player = moveState (player game)
    }
    where moveState = if key `elem` upButtons
            then (case state of
                Up      -> down
                Down    -> up)
            else if key `elem` downButtons
            then (case state of
                Up      -> up
                Down    -> down)
            else id


addEvent :: Float -> (Game -> Game) -> Game -> Game
addEvent marker func game =
        game{events=newMap}
    where
        newMap = Map.insertWith (++) marker [func] (events game)


runEvents :: Game -> Game
runEvents game@Game{..} =
        Map.foldr (foldr (.) id ) restGame consumed
    where   
        (consumed, rest) = x `Map.split` events
        (x,_) = position player 
        restGame = game{events = rest}


render :: Game -> Picture
render GameOver{view=view@View{..},..} = pictures [background, endscreen]
    where
        background = translate (-(left view)/2) (-(bottom view)/2) $ rectangleSolid width height
        endscreen = color white $ translate (-400) (-50) $ text "Game Over!"


render game@Game{..} = do
    --let background = drawWorld (world game) (rectangle game)
    let planes  = map draw otherPlanes ++ [draw player]
    let scene   = pictures planes
    let (x,y)   = negV $ position player

    pictures [drawWorld view, scale 20 20 $ translate x y $ pictures [scene]]

    where
        pos = position player
        draw    = drawPlane 
        bgRect  = color (greyN 0.5) $!
             rectangleSolid (width view) (height view)



data Coin = Bronze | Silver | Gold
coinToScore Bronze  = 5
coinToScore Silver  = 10
coinToScore Gold    = 30


addCoin :: Coin -> Point -> Game -> Game
addCoin coin point@(x,y) =
    addEvent x f
    where
        f game = if playerPos game < 10 then got game else game
        got game = game { score = score game + coinToScore coin }


update :: Float -> Game -> IO Game
update time game@GameOver{} = return game

update time game@Game{..} = do
    if collides World newPlayer then
        return GameOver{view=View{pos=(0,0),width=width view,height=height view}}
    else return $ runEvents $ game
        { player=newPlayer
        , otherPlanes = updatePlane time `map` otherPlanes
        , view=view {pos = position newPlayer}
        }
    where
        newPlayer = updatePlane time player


main = do
    let window = InWindow "Ludum Dare 34" (100, 100) (windowWidthI, windowHeightI)

    game <- newGame
    let game2 = addCoin Bronze (20,10) game

    playIO window white 60 game2 (return.render) (\a b -> return $ input a b) update


