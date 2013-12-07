module Rogue.World 
    (
      viewTiles
    , positionPlayer
    , inWorld
    ) where

import Rogue.Types
import Rogue.Util
import Rogue.WorldGen

import qualified Data.Set as S

import Data.Array
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Lens

viewTiles :: Position -> Rogue ()
viewTiles p1@(x,y) = do
    w <- use world 
    r <- view viewRadius
    visible .= S.empty
    mapM_ viewTile (visibleTiles p1 r w)

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

positionPlayer :: Rogue ()
positionPlayer = do
    w <- use world
    let possiblePositions = map fst $ filter (isFloor . snd) $ assocs w
    unless (null possiblePositions) $ do
        newPos <- randElemR possiblePositions
        player.position .= newPos
        viewTiles newPos
