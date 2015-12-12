
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Graphics.Gloss.Data.Point

import Plane
import Util
import World


windowWidthI = 800              :: Int
windowHeightI = 600             :: Int



data Game = Game
    {widthI         :: !Int
    ,heightI        :: !Int
    ,movement       :: !PlaneMovement
    ,player         :: !Plane
    ,otherPlanes    :: [Plane]
    }
    | GameOver

width = fromIntegral . widthI       :: Game -> Float
height = fromIntegral . heightI     :: Game -> Float

newGame :: IO Game
newGame = return $ Game
    {widthI     = windowWidthI
    ,heightI    = windowHeightI
    ,movement   = Hold
    ,player     = Plane {position=(0,0),velocity=(1,0),angle=0,movement=Hold}
    ,otherPlanes =
        [Plane {position=(0,5),velocity=(0.8,0),angle=0,movement=Hold}
        ]
    }


input :: Event -> Game -> IO Game
input event GameOver = return GameOver
input event game = do
    return $ case event of
        EventResize(w,h)            -> game{widthI=w,heightI=h}
        (EventKey key state _ _)    -> keyPress key state game
        _                           -> game


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


sky = png "images/background.png"
-- grass = scale (1/20) (1/20) $ png "images/background_grass.png"


render :: Game -> Picture
render GameOver = pictures [background, endscreen]
    where
        background = rectangleSolid 10000 10000
        endscreen = color white $ translate (-400) (-50) $ text "Game Over!"


render game@Game{..} = do
    --let background = drawWorld (world game) (rectangle game)
    let planes  = map draw otherPlanes ++ [draw player]
    let scene   = pictures planes
    let (x,y)   = negV $ position player

    pictures [sky, scale 20 20 $ translate x y $ pictures [scene]]

    where
        pos = position player
        draw    = drawPlane 
        bgRect  = color (greyN 0.5) $!
             rectangleSolid (width game) (height game)



update :: Float -> Game -> IO Game
update time GameOver = return GameOver

update time game@Game{..} = do
    if collides World newPlayer then
        return GameOver
    else return $ game
        { player=newPlayer
        , otherPlanes = updatePlane time `map` otherPlanes
        }
    where
        newPlayer = updatePlane time player


main = do
    let window = InWindow "Ludum Dare 34" (100, 100) (windowWidthI, windowHeightI)

    game <- newGame

    playIO window white 60 game (return.render) input update


