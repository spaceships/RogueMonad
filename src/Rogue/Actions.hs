module Rogue.Actions
    (
      move
    , quit
    ) where

import Rogue.Types
import Rogue.Util (addP)
import Rogue.World (inWorld)

import Data.Array ((!))
import Control.Monad.State (get, put, modify)
import Control.Monad (when)
import Control.Lens
import qualified Data.Map as M

move :: Direction -> Rogue ()
move d = do
    st <- get

    let w      = st^.world
        es     = st^.enemies
        p      = st^.player
        fs     = st^.floors
        newPos = (p^.position) `addP` dirToPos d

    when (newPos `elem` fs && newPos `M.notMember` es) $ do
        player.position .= newPos
        
isFloor :: Position -> World -> Bool
isFloor = isThing Floor

isThing :: Thing -> Position -> World -> Bool
isThing t p w 
    | p `inWorld` w = w ! p == t
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
quit = done .= True
