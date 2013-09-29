module Rogue.Interface where

import Rogue.Types
import Rogue.World
import Rogue.Actions

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Text.Printf
import System.Console.ANSI
import System.IO (stdin, hSetEcho)
import Data.Maybe (fromMaybe)


rogue :: Rogue ()
rogue = do
    liftIO hideCursor
    liftIO clearScreen
    liftIO $ hSetEcho stdin False

    catchError play (\_ -> return ())

    liftIO $ hSetEcho stdin True
    liftIO showCursor

play :: Rogue ()

play = do
    keys <- asks bindings
    update
    k <- liftIO getChar
    fromMaybe (return ()) (lookup k keys)
    play

update :: Rogue ()
update = do
    liftIO $ setCursorPosition 0 0
    status <- getStatusBar
    world  <- showWorld
    liftIO $ putStr (status ++ world)


getStatusBar :: Rogue String
getStatusBar = do
    Actor hp maxHp acc def _ _ g <- gets player
    (width, _) <- asks screenSize
    let info = printf "| %c | acc:%d | def: %d | hp: %d/%d |" g acc def hp maxHp
    return $ center info width

center :: String -> Int -> String
center s w = replicate left '-' ++ s ++ replicate right '-' ++ "\n"
  where
    l = length s
    toBeFilled = w - l
    (both, leftAdd) = toBeFilled `divMod` 2
    (left, right) = (both + leftAdd, both)
