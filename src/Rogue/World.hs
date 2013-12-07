module Rogue.World 
    ( viewTiles
    , inWorld
    , positionPlayer
    , positionPlayerRandomly
    , newLowerLevel
    ) where

import Rogue.Types
import Rogue.Util
import Rogue.WorldGen

import qualified Data.Set as S

import Data.Array
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Lens hiding (Level)

viewTiles :: Position -> Rogue ()
viewTiles p1@(x,y) = do
    w <- use $ currentLevel.world
    r <- view viewRadius
    currentLevel.visible .= S.empty
    mapM_ viewTile (visibleTiles p1 r w)

viewTile :: Position -> Rogue ()
viewTile p = do
    currentLevel.seen    %= S.insert p
    currentLevel.visible %= S.insert p

visibleTiles :: Position -> Int -> World -> [Position]
visibleTiles p1 r w = S.toList $ execState addPositions S.empty
  where
    endPts = circlePoints' p1 r
    addPositions :: State (S.Set Position) ()
    addPositions = forM_ endPts $ \p2 -> do 
        let seg = segment p1 p2 r
            ps  = takeWhile' (\p -> inWorld p w && (isFloor $ w ! p)) seg
        mapM_ (\p -> modify (S.insert p)) ps

positionPlayer :: Position -> Rogue ()
positionPlayer pos = do
    player.position .= pos
    viewTiles pos

positionPlayerRandomly :: Rogue ()
positionPlayerRandomly = do
    w <- use $ currentLevel.world
    let pos = map fst $ filter (isFloor . snd) $ assocs w
    if not (null pos) then do
        newPos <- randElemR pos
        player.position .= newPos
        viewTiles newPos
    else
        alert "error: no floor found for player" >> wait >> exitGameNow .= True

newLowerLevel :: Rogue ()
newLowerLevel = do
    w <- randomWorldR 
    lowerLevels %= ((emptyLevel & world .~ w) :)
    linkStairs

-- Link the stairs of the current and next levels
linkStairs :: Rogue ()
linkStairs = do
    w1 <- use $ currentLevel.world
    nextLevels <- use lowerLevels
    let nextLevel = head nextLevels
        w2 = nextLevel ^. world
        downStairs = filter (isDownStair . snd) $ assocs w1
        upStairs = filter (isUpStair . snd) $ assocs w2
        pairings = zip downStairs upStairs
    forM_ pairings $ \((p1,_),(p2,_)) -> do
        currentLevel.stairsDown.at p1 ?= p2
        lowerLevels.element 0.stairsUp.at p2 ?= p1
