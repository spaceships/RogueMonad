module Rogue.Actions where

import Rogue.Types
import Rogue.World

import Data.Array
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M

move :: Direction -> Rogue ()
move d = do
    st <- get

    let w      = world st
        es     = enemies st
        p      = player st
        newPos = pos p `addP` dirToPos d

    when (newPos `isFloor` w && newPos `M.notMember` es)
        (put st{ player = p{ pos = newPos }})

isFloor :: Position -> World -> Bool
isFloor = isThing Floor

isThing :: Thing -> Position -> World -> Bool
isThing t p w 
    | p `inWorld` w = maybe False (== t) (w ! p)
    | otherwise     = False          

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

quit :: Rogue ()
quit = throwError $ error "Quit"
