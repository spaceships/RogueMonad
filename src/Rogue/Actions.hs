module Rogue.Actions
    (
      move
    , quit
    , positionPlayer
    ) where

import Rogue.Types
import Rogue.Util
import Rogue.World (inWorld)

import Data.List (delete)
import Data.Array
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Lens
import qualified Data.Map as M

positionPlayer :: Rogue ()
positionPlayer = do
    w <- use world
    let possiblePositions = map fst $ filter (isFloor . snd) $ assocs w
    unless (null possiblePositions) $ do
        newPos <- randElemR possiblePositions
        player.position .= newPos

move :: Direction -> Rogue ()
move d = void $ runMaybeT $ do
    s <- lift get
    let w = s^.world
        newPos = (s^.player.position) `addP` dirToPos d
    guard (newPos `inWorld` w) 
    guard (isFloor (w ! newPos))
    guard (not $ anyOf (enemies.traverse.position) (== newPos) s)
    player.position .= newPos
        
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
quit = exitGame .= True
