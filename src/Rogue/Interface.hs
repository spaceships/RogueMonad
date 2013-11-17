module Rogue.Interface 
    (
      rogue
    ) where

import Rogue.Types
import Rogue.Util (center)
import Rogue.World (createWorld, positionPlayer, showWorld)

import Control.Monad.State (get, gets)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import Control.Monad (unless)
import Control.Lens
import Text.Printf (printf)
import System.Console.ANSI (hideCursor, clearScreen, showCursor, setCursorPosition)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering), hSetEcho)
import Data.Maybe (fromMaybe)

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
    keys <- view bindings
    update
    k <- liftIO getChar
    fromMaybe (return ()) (lookup k keys)
    play

untilQuit :: Rogue () -> Rogue ()
untilQuit m = use done >>= \d -> unless d m

update :: Rogue ()
update = do
    liftIO $ setCursorPosition 0 0
    status <- getStatusBar
    world  <- showWorld
    liftIO $ putStr (status ++ world)

getStatusBar :: Rogue String
getStatusBar = do
    Actor hp maxHp acc def position@(x,y) _ g <- use player
    (width, _) <- view screenSize
    let info = printf "| %c | acc:%d | def: %d | hp: %d/%d | (%d,%d) |" g acc def hp maxHp x y
    return $ center info width ++ "\n"

