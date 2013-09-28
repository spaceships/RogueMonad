module Rogue.World where

import Rogue.Types

import Control.Monad.State
import Control.Monad.Reader
import Data.Array
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text as T

showWorld :: Rogue T.Text
showWorld = do
    s <- get
    c <- ask
    let w  = world s
        es = enemies s
        p  = player s

        (sMaxX, sMaxY) = screenSize c
        glyphs         = worldGlyphs c
        
    let (x, y)       = pos p
        (dx, dy)     = (sMaxX `div` 2, sMaxY `div` 2)
        (xmin, ymin) = (x - dx, y - dy)
        (xmax, ymax) = (x + dx, y + dy)
        textWorld    = fmap (toGlyph glyphs) w :: Array Position Char
        things       = Map.insert (pos p) p es

    return $ genTextWorld textWorld things (xmin, ymin) (xmax, ymax)

genTextWorld :: Array Position Char -> 
                Map.Map Position Actor -> 
                Position -> 
                Position -> 
                T.Text

genTextWorld w things (xmin, ymin) (xmax, ymax) = 
    T.unlines [ getLine y | y <- [ymin..ymax] ]
  where
    getLine y = foldr T.cons (T.pack "") $ do
        x <- [xmin..xmax]
        return $ case Map.lookup (x,y) things of
            Nothing -> if x > 0 && y > 0 then w ! (x,y) else ' '
            Just t  -> glyph t
        
toGlyph :: WorldGlyphMap -> Maybe Thing -> Char
toGlyph gm (Just x) = fromMaybe ' ' $ Map.lookup x gm
toGlyph _ Nothing   = ' '

blankWorld :: Size -> World
blankWorld s@(maxX, maxY) = 
    array ((0,0), s) [ ((x,y), Just Floor) | x <- [0..maxX], y <- [0..maxY] ]
