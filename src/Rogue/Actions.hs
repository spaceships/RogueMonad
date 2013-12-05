module Rogue.Actions
    (
      move
    , quit
    , positionPlayer
    ) where

import Rogue.Types
import Rogue.Util (isFloor, randElemR, addP, circlePoints', segment, takeWhile')
import Rogue.World (inWorld)

import Data.List (delete)
import Data.Array (assocs, (!))
import Control.Monad (unless, void, guard, forM_)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Lens

positionPlayer :: Rogue ()
positionPlayer = do
    w <- use world
    let possiblePositions = map fst $ filter (isFloor . snd) $ assocs w
    unless (null possiblePositions) $ do
        newPos <- randElemR possiblePositions
        player.position .= newPos
        viewTiles newPos

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
quit = exitGame .= True

viewTiles :: Position -> Rogue ()
viewTiles p1@(x,y) = do
    w <- use world 
    r <- view viewRadius
    visible .= S.empty
    mapM_ viewTile (visibleTiles p1 r w)

circle :: Position -> Int -> World -> S.Set Position
circle p r w = S.fromList $ circlePoints' p r

viewTile :: Position -> Rogue ()
viewTile p = do
    seen %= S.insert p
    visible %= S.insert p

visibleTiles :: Position -> Int -> World -> [Position]
visibleTiles p1 r w = S.toList $ execState addPositions S.empty
  where
    endPts = circlePoints' p1 r
    addPositions :: State (S.Set Position) ()
    addPositions = forM_ endPts $ \p2 -> do 
        let seg = segment p1 p2 r
        let ps = takeWhile' (\p -> inWorld p w && (isFloor $ w ! p)) seg
        mapM_ (\p -> modify (S.insert p)) ps
