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


module View
    (View(..)
    ,getRectangle
    ,left,bottom,right,top
    ,repeatX,repeatY
    ,resize
) where


import Graphics.Gloss.Data.Vector
import Graphics.Gloss
import Data.Fixed (mod')

data View = View
    { pos           :: !Vector
    , width         :: !Float
    , height        :: !Float
    }

getRectangle :: View -> (Float, Float, Float, Float)
getRectangle (View (x,y) w2 h2) = (x-w,y-h,x+w,y+h)
    where
        w = w2/2
        h = h2/2

left (View (x,_) _ _)   = x
bottom (View (_,y) _ _) = y
right view  = left view + width view
top view    = bottom view + height view


resize :: (Int, Int) -> View -> View
resize (w,h) view = view { width=fromIntegral w, height=fromIntegral h}

repeatX :: Float -> View -> Picture -> Picture
repeatX tileSize view pic = translate ((-left view)*20-width view) (-(bottom view)*20) lst
    where
        start   = floor $ left view*20/tileSize     :: Int
        end     = ceiling $ right view*20/tileSize  :: Int
        lst     = pictures [translate (fromIntegral s*tileSize) 0 pic | s<-[start..end]]


repeatY :: Float -> View -> Picture -> Picture
repeatY tileSize view pic = translate (-(left view)*20) (-(bottom view)*20-height view) lst
    where
        start   = floor $ bottom view*20/tileSize   :: Int
        end     = ceiling $ top view*20/tileSize    :: Int
        lst     = pictures [translate 0 (fromIntegral s*tileSize) pic | s<-[start..end]]

