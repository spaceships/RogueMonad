module Rogue.Actions
    ( move
    , genNewWorld
    , goDownstairs
    , goUpstairs
    , promptExitDungeon
    , getPosition
    ) where

import Rogue.Types
import Rogue.Util
import Rogue.World
import Rogue.WorldGen

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Lens hiding (Level)
import Graphics.Vty

move :: Direction -> Rogue ()
move d = void $ runMaybeT $ do
    s <- lift get
    let w = s^.currentLevel.world
        newPos = (s^.player.position) `addP` dirToPos d
    guard (isJustFloor $ w ^. at newPos)
    guard (not $ anyOf (currentLevel.enemies.traverse.position) (== newPos) s)
    lift $ positionPlayer newPos

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

-- wipe everything!
genNewWorld :: Rogue ()
genNewWorld = do
    g <- use stdGenR
    w <- randomWorldR
    currentLevel .= (emptyLevel & world .~ w)
    upperLevels .= []
    lowerLevels .= []
    depth .= 1
    positionPlayerRandomly

getPosition :: Rogue ()
getPosition = do
    pos <- use $ player.position
    d <- use depth
    alert $ "Depth: " ++ show d ++ "\nPosition: " ++ show pos
    wait

getStairsInfo :: Rogue ()
getStairsInfo = do
    wait

goDownstairs :: Rogue ()
goDownstairs = do
    w <- use $ currentLevel.world
    pos <- use $ player.position
    when (isJustDownStair $ w ^. at pos) goDown

goDown :: Rogue ()
goDown = do
    lows <- use lowerLevels         -- generate a new level, if there isn't one already
    when (null lows) newLowerLevel

    lows <- use lowerLevels         -- lowerLevels may have been modified
    l <- use currentLevel

    depth += 1
    currentLevel .= head lows
    upperLevels  %= (l :)
    lowerLevels  %= tail

    oldPos <- use $ player.position
    let newPos = fromMaybe (0,0) $ l ^. stairsDown.at oldPos
    positionPlayer newPos

goUpstairs :: Rogue ()
goUpstairs = do
    w <- use $ currentLevel.world
    pos <- use $ player.position
    d <- use depth
    if (d == 1) then
        promptExitDungeon
    else
        when (isJustUpStair $ w ^. at pos) goUp

goUp :: Rogue ()
goUp = do
    ups <- use upperLevels
    l <- use currentLevel

    depth -= 1
    currentLevel .= head ups
    lowerLevels  %= (l :)
    upperLevels  %= tail

    oldPos <- use $ player.position
    let newPos = fromMaybe (0,0) $ l ^. stairsUp.at oldPos
    positionPlayer newPos

promptExitDungeon :: Rogue ()
promptExitDungeon = do
    alert "Are you sure you want to leave the dungeon? [Y/n]"
    v <- view term  
    e <- liftIO $ next_event v
    case e of
        EvKey (KASCII 'y') _ -> quit
        EvKey (KASCII 'Y') _ -> quit
        EvKey KEnter _       -> quit
        EvKey (KASCII 'n') _ -> return ()
        _                    -> promptExitDungeon
