module Rogue.World where

import Rogue.Types

import Control.Monad.State
import Control.Monad.Reader
import Data.Array
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

showWorld :: Rogue String
showWorld = do
    st  <- get
    cfg <- ask

    let p         = player st
        dPos      = liftP (`div` 2) (screenSize cfg)
        screenMin = pos p `subP` dPos
        screenMax = pos p `addP` dPos
        textArray = fmap (toGlyph (worldGlyphs cfg)) (world st)
        things    = M.insert (pos p) p (enemies st)
        str       = textWorld textArray things screenMin screenMax

    return str

textWorld :: Array Position Char -> 
             M.Map Position Actor -> 
             Position -> 
             Position -> 
             String
textWorld w things (xmin, ymin) (xmax, ymax) = 
    unlines [ makeLine y | y <- [ymin..ymax] ]
  where
    makeLine y = [ getChar (x,y) | x <- [xmin..xmax] ]
    getChar p | p `inWorld` w = maybe (w ! p) glyph $ M.lookup p things 
              | otherwise     = ' '
        
toGlyph :: WorldGlyphMap -> Maybe Thing -> Char
toGlyph gm (Just x) = fromMaybe ' ' $ M.lookup x gm
toGlyph _ Nothing   = ' '

blankWorld :: Size -> World
blankWorld size@(maxX, maxY) = 
    array ((0,0), size) [ ((x,y), Just Floor) | x <- [0..maxX], y <- [0..maxY] ]

inWorld :: Position -> Array (Int,Int) a -> Bool
inWorld (x,y) w = 
    x >= 0 && 
    y >= 0 && 
    x <= maxX && 
    y <= maxY
  where
    (_, (maxX, maxY)) = bounds w

