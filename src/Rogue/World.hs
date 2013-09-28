module Rogue.World where

import Rogue.Types
import Data.Array
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text as T

showWorld :: Rogue T.Text
showWorld = do
    RState world enemies player <- get
    RConfig worldSize screenSize glyphs <- ask
    let viewCenter = fmap (`div` 2) screenSize
        textWorld = fmap (toGlyph glyphs) world :: Array Position Char

    return $ T.pack ""

genTextWorld :: Array Position Char -> 
                Map.Map Position Actor -> 
                Position -> 
                Position -> 
                T.Text

genTextWorld w things (xmin, ymin) (xmax, ymax) = 
    T.unlines [ getLine y | y <- [ymin..ymax] ]
  where
    getLine y = foldr T.cons (T.pack "") [
        case Map.lookup (x,y) things of
            Nothing -> w ! (x,y) -- no actor here
            Just t  -> glyph t   -- actor here, show its glyph
            | x <- [xmin..xmax]  ]
        
toGlyph :: WorldGlyphMap -> Maybe Thing -> Char
toGlyph gm (Just x) = fromMaybe ' ' $ Map.lookup x gm
toGlyph _ Nothing   = ' '

blankWorld :: Size -> World
blankWorld s@(maxX, maxY) = 
    array ((0,0), s) [ ((x,y), Just Floor) | x <- [0..maxX], y <- [0..maxY] ]
