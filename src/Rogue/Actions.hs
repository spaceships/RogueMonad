module Rogue.Actions where

import Rogue.Types
import Rogue.World

import Data.Array ((!))
import qualified Data.Map as Map (lookup)
import Control.Monad (guard)
import Control.Monad.State
import Control.Monad.Reader

move :: Direction -> Rogue ()
move d = do
    s <- get
    (maxX, maxY) <- asks worldSize

    let w = world s
        es = enemies s
        p = player s
        newPos@(x,y) = dirToPos d `addP` pos p

        -- check that new pos is IN THE WORLD
    if (x >= 0 && y >= 0 && x <= maxX && y <= maxY) &&
        -- check that new pos is FLOOR
       (maybe False (== Floor) (w ! newPos)) &&
        -- check that there is no enemy there
       (maybe True (const False) $ Map.lookup newPos es)
    then -- MOVE
        put $ s { player = p { pos = newPos } }
    else
        return ()

dirToPos :: Direction -> Position
dirToPos d = case d of 
    N  -> ( 0,-1)
    NE -> ( 1,-1)
    E  -> ( 1, 0)
    SE -> ( 1, 1)
    S  -> ( 0, 1)
    SW -> (-1, 1)
    W  -> (-1, 0)
    NW -> (-1,-1)
