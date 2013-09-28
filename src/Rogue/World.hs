module Rogue.World where

import Rogue.Types
import Data.Array
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text as T

showWorld :: Rogue T.Text
showWorld = do
    RState world enemies player <- get
    RConfig worldSize (screenMaxX, screenMaxY) glyphs <- ask
    let (x, y)       = pos player
        (dx, dy)     = (screenMaxX `div` 2, screenMaxY `div` 2)
        (xmin, ymin) = (x - dx, y - dy)
        (xmax, ymax) = (x + dx, y + dy)
        textWorld    = fmap (toGlyph glyphs) world :: Array Position Char
        things       = Map.fromList [ (pos player, player) ] -- add enemies eventually

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
