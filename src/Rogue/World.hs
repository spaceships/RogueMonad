module Rogue.World 
    ( viewTiles
    , positionPlayer
    , positionPlayerRandomly
    , newLowerLevel
    ) where

import Rogue.Types
import Rogue.Util
import Rogue.WorldGen

import qualified Data.Set as S

import Data.Functor
import Data.Maybe (isJust, fromJust, mapMaybe)
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
        let f i = let t = w^.at i in if isJust t then Just (i, fromJust t) else Nothing
            seg = mapMaybe f $ segment p1 p2 r
            ps  = takeWhile' (isFloor.snd) seg
        mapM_ (modify . S.insert . fst) ps

positionPlayer :: Position -> Rogue ()
positionPlayer pos = do
    player.position .= pos
    viewTiles pos

positionPlayerRandomly :: Rogue ()
positionPlayerRandomly = do
    possiblePlaces <- gets (^..currentLevel.world.itraversed.filtered isFloor.asIndex)
    if not (null possiblePlaces) then do
        newPos <- randElemR possiblePlaces
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
    downStairs <- gets (^..currentLevel.world.itraversed.filtered isDownStair.asIndex)
    upStairs   <- gets (^..lowerLevels.element 0.world.itraversed.filtered isUpStair.asIndex)
    let pairings = zip downStairs upStairs
    forM_ pairings $ \(p1,p2) -> do
        currentLevel.stairsDown.at p1        ?= p2
        lowerLevels.element 0.stairsUp.at p2 ?= p1
