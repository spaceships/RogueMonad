module Rogue.Interface where

import Rogue.Types
import Rogue.World
import Rogue.Actions

import Control.Monad.State
import Control.Monad.Reader
import Text.Printf
import System.Console.ANSI
import System.IO
import Data.Maybe



rogue :: Rogue ()
rogue = do
    liftIO setTermOpts
    createWorld
    positionPlayer
    play
    liftIO unsetTermOpts

setTermOpts :: IO ()
setTermOpts = do 
    hSetBuffering stdin NoBuffering
    hideCursor
    clearScreen
    hSetEcho stdin False

unsetTermOpts :: IO ()
unsetTermOpts = do
    hSetEcho stdin True
    showCursor

play :: Rogue ()
play = untilQuit $ do
    keys <- asks bindings
    update
    k <- liftIO getChar
    fromMaybe (return ()) (lookup k keys)
    play

untilQuit :: Rogue () -> Rogue ()
untilQuit m = gets done >>= \d -> unless d m

update :: Rogue ()
update = do
    liftIO $ setCursorPosition 0 0
    status <- getStatusBar
    world  <- showWorld
    liftIO $ putStr (status ++ world)


getStatusBar :: Rogue String
getStatusBar = do
    Actor hp maxHp acc def position _ g <- gets player
    (width, _) <- asks screenSize
    let info = printf "| %c | acc:%d | def: %d | hp: %d/%d | (%d,%d) |" g acc def hp maxHp (fst position) (snd position)
    return $ center info width ++ "\n"

center :: String -> Int -> String
center s w = replicate left ' ' ++ s ++ replicate right ' '
  where
    l = length s
    toBeFilled = w - l
    (both, leftAdd) = toBeFilled `divMod` 2
    (left, right) = (both + leftAdd, both)

progressBar :: String -> Int -> Rogue (Int -> Rogue ())
progressBar label total = do
    liftIO clearScreen
    (maxX,maxY) <- asks screenSize
    let length = 20
        y = maxY `div` 2
    liftIO $ setCursorPosition (y-1) 0
    liftIO $ putStrLn (center label maxX)
    return $ \n -> do
        liftIO $ setCursorPosition y 0
        let p = floor ((fromIntegral n / fromIntegral total) 
                        * fromIntegral length)
            s = "|" ++ replicate p '=' ++ replicate (length - p) ' ' ++ "|"
        liftIO $ putStr $ center s maxX
        
createWorld :: Rogue ()
createWorld = do
    clearWorld
    makeInitialRoom
    makeTunnels

makeTunnels :: Rogue ()
makeTunnels = do
    max <- asks numTunnels
    bar <- progressBar "generating map" max
    let helper n = when (n < max) $ do
            bar n
            walls <- gets walls
            wallsWithAdjacentFloors <- filterM adjacentFloor walls
            theWall <- randElem wallsWithAdjacentFloors
            dir <- tunnelDirection theWall
            when (isJust dir) $ do
                save <- get
                okOnly <- asks onlyTerminalTunnels
                ok <- tunnel theWall $ fromJust dir
                when (okOnly && not ok) $ put save
            helper (n + 1)
    helper 0
