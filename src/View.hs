
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

