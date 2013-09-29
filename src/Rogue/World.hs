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
    
    ((xmin,ymin), (xmax, ymax)) <- screenDimensions

    let
        p                = player st
        actors           = M.insert (position p) p (enemies st)
        getChar          = getCharAtPos actors (world st) (worldGlyphs cfg)
        makeLine y       = [ getChar (x,y) | x <- [xmin..xmax] ]
        textWorld cfg st = unlines [ makeLine y | y <- [ymin..ymax] ]

    return $ textWorld cfg st


        
getCharAtPos :: M.Map Position Actor -> World -> WorldGlyphMap -> Position -> Char
getCharAtPos things w gm pos = 
    if pos `inWorld` w then 
        maybe (toGlyph gm $ w ! pos) glyph $ M.lookup pos things 
    else
        ' ' 

screenDimensions :: Rogue (Position, Position)
screenDimensions = do
    p <- gets (position . player)
    size <- asks screenSize
    let dPos = liftP (`div` 2) size
    return (p `subP` dPos, p `addP` dPos)


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

