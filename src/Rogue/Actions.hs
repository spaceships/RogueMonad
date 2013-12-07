module Rogue.Actions
    (
      move
    , quit
    , genNewWorld
    ) where

import Rogue.Types
import Rogue.Util
import Rogue.World
import Rogue.WorldGen

import Data.Array ((!))
import Control.Monad (void, guard)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Lens

move :: Direction -> Rogue ()
move d = void $ runMaybeT $ do
    s <- lift get
    let w = s^.world
        newPos = (s^.player.position) `addP` dirToPos d
    guard (newPos `inWorld` w) 
    guard (isFloor (w ! newPos))
    guard (not $ anyOf (enemies.traverse.position) (== newPos) s)
    player.position .= newPos
    lift $ viewTiles newPos
        
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
quit = exitGameNow .= True

genNewWorld :: Rogue ()
genNewWorld = do
    g <- use stdGenR
    world .= randomWorld g
    seen .= S.empty
    positionPlayer

