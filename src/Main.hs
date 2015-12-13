
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Graphics.Gloss.Data.Point
import Control.Monad.State


import Plane
import Util
import World
import View
import Game


windowWidthI = 800              :: Int
windowHeightI = 600             :: Int

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



main = do
    let window = InWindow "Ludum Dare 34" (100, 100) (windowWidthI, windowHeightI)

    game <- newGame windowWidthI windowHeightI
    let game2 = execState (do
            addCoin Bronze (50,10)
            ) game

    playIO window white 60 game2 (return.render) (\a b -> return $ input a b) update


