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
import Level
import Multiline


windowWidthI = 800              :: Int
windowHeightI = 600             :: Int

input :: Event -> Game -> IO Game

-- special key for logging position (useful for tutorial levels)
input (EventKey (SpecialKey KeySpace) Up _ _)   game@Game{..} = do
    print $ printInt x ++ ", " ++ printInt y
    return game
    where
        (x,y) = position player
input (EventResize s) game          = return $ game{view=resize s (view game)}
input (EventKey key state _ _) game@Game{} = return $ keyPress key state game
input (EventKey (SpecialKey key) Up _ _) game@GameOver{..} =
    return $ case key of
        KeyUp   -> game{level=level+1}
        KeyDown -> game{level=max 0 $ level-1}
        KeyEnter -> let game = startGame view level
                    in execState (loadLevel level) game
        _       -> game
input event game = return $ game


printInt :: Float -> String
printInt a = show (round a)




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



drawUI fuel score view = translate ((-width view)/2+size+10) ((-height view)/2+size+10) graphic
    where
        size    = 64
        graphic = pictures [scorePrint, txt, bg, border, marker, arrow]
        txt     = translate (-size) (size+10) $ scale 0.2 0.2 $ text $ "Fuel: " ++ show (round fuel) ++ " sec"
        bg      = color white $ circleSolid size
        border  = color black $ thickCircle size 2
        arrow   = rotate (-(min (fuel/30*180) 300 )) $  color red $ line [(0,0), (size,0)]
        marker  = color black $ line [(0,0), (size,0)]
        scorePrint = translate (size+10) (-size/2-10) $ scale 0.2 0.2 $ text $ "Score: " ++ show score


-- drawScore


render :: Game -> Picture
render GameOver{view=view@View{..},..} = pictures [background, introduction, endscreen, next]
    where
        background = translate (-(left view)/2) (-(bottom view)/2) $ rectangleSolid width height
        endscreen = if first then blank else color white $ translate (-400) 200 $ multiline [mainText, scoreS]
        mainText = if victory then "Target reached!" else "Failed!"
        scoreS = "Score: " ++ show score
        next = color white $ translate (-400) (-200) $ scale 0.2 0.2 $ multiline
            ["Next level: " ++ show level
            , "press Up for next, Down for previous"
            , "Start on Enter"]
        introduction = translate (-400) (-50) $ color white $ scale 0.2 0.2 $ multiline
            ["Press Up and Down to turn the plane"
            ,"Don't run out of fuel"
            ,"Avoid the bombs"
            ,"Too much fuel will make you too heavy"]

render game@Game{..} = do
    --let background = drawWorld (world game) (rectangle game)
    let (x,y)   = negV $ position player

    pictures [drawWorld view, scale 20 20 $ translate x y scene, ui]

    where
        ui      = drawUI (fuel player) score view
        scene   = evalState drawIngame game
        pos     = position player
        bgRect  = color (greyN 0.5) $!
             rectangleSolid (width view) (height view)



main = do
    let window = InWindow "Ludum Dare 34" (windowWidthI, windowHeightI) (100, 100)

    let game = newGame windowWidthI windowHeightI

    playIO window white 60 game (return.render) (\a b -> input a b) update


