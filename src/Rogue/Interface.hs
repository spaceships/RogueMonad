module Rogue.Interface 
    (
      rogue
    , genWorld
    ) where

import Rogue.Types
import Rogue.Util
import Rogue.World
import Rogue.Actions

import Data.Maybe
import Data.Array
import qualified Data.Map as M
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad (unless)
import Control.Lens
import Text.Printf (printf)
import System.Console.ANSI (hideCursor, clearScreen, showCursor, setCursorPosition)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering), hSetEcho)
import Data.Maybe (fromMaybe)

rogue :: Rogue ()
rogue = do
    liftIO setTermOpts
    genWorld
    play
    liftIO unsetTermOpts

genWorld :: Rogue ()
genWorld = do
    g <- use stdGenR
    world .= randomWorld g
    positionPlayer

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
untilQuit m = use exitGame >>= \d -> unless d m

update :: Rogue ()
update = do
    liftIO $ setCursorPosition 0 0
    status <- getStatusBar
    world  <- showWorld
    liftIO $ putStr (status ++ world)

getStatusBar :: Rogue String
getStatusBar = do
    p <- use player
    gs <- view glyphs
    let playerGlyph = fromMaybe ' ' $ M.lookup "Player" gs
    width <- view (screenSize._1)
    let info = printf "| %c | acc:%d | def: %d | hp: %d/%d | (%d,%d) |" playerGlyph (p^.acc) (p^.def) (p^.hp) (p^.maxHp) (p^.position._1) (p^.position._2)
    return $ center info width ++ "\n"

showWorld :: Rogue String
showWorld = do
    s   <- get
    cfg <- lift ask
    ((xmin,ymin), (xmax, ymax)) <- screenDimensions
    let
        actors     = (s^.player) : (s^.enemies)
        getChar    = getCharAtPos actors (s^.world) (cfg^.glyphs)
        makeLine y = [ getChar (x,y) | x <- [xmin..xmax] ]
        textWorld  = unlines [ makeLine y | y <- [ymin..ymax] ]
    return $ textWorld

getCharAtPos :: [Actor] -> World -> GlyphMap -> Position -> Char
getCharAtPos actors w gm pos  
    -- Case 1: There is an actor at pos: display actor's glyph
    | pos `inWorld` w && isJust actorAtPos = 
        let actor = fromJust actorAtPos 
        in fromMaybe '!' $ gm ^? ix (actor ^. name)
            
    -- Case 2: pos is in the world: display the thing there
    | pos `inWorld` w = fromMaybe ' ' $ M.lookup (simplify $ w ! pos) gm

    -- Case 3: pos is not in world: display a space
    | otherwise = ' ' 
  where 
    actorAtPos = actors ^? traversed.filtered (\a-> a^.position == pos)
    simplify (Floor [] Nothing)    = "Floor"
    simplify (Floor (x:_) Nothing) = show x
    simplify (Floor _ (Just s))    = show s
    simplify x = show x

screenDimensions :: Rogue (Position, Position)
screenDimensions = do
    p <- use (player.position)
    size <- view screenSize
    let dPos = liftP (`div` 2) size
    return (p `subP` dPos, p `addP` dPos)

