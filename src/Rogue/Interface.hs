module Rogue.Interface 
    (
      rogue
    , genWorld
    ) where

import Rogue.Types
import Rogue.Util
import Rogue.World
import Rogue.Actions

import Data.List
import Data.Maybe
import Data.Array
import qualified Data.Map as M
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
import Control.Lens
import Text.Printf (printf)
import System.Console.ANSI
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

-- Prints the current world
update :: Rogue ()
update = do
    liftIO $ setCursorPosition 0 0
    status <- getStatusBar
    liftIO $ putStr status
    world <- showWorld
    liftIO $ sequence_ world

getStatusBar :: Rogue String
getStatusBar = do
    p <- use player
    gs <- view glyphs
    let playerGlyph = (gs ^?! ix "Player") ^. glyph
    width <- view (screenSize._1)
    let info = printf "| %c | acc:%d | def: %d | hp: %d/%d | (%d,%d) |" playerGlyph (p^.acc) (p^.def) (p^.hp) (p^.maxHp) (p^.position._1) (p^.position._2)
    return $ center info width ++ "\n"

showWorld :: Rogue [IO ()]
showWorld = do
    s   <- get
    cfg <- lift ask
    ((xmin,ymin), (xmax, ymax)) <- screenDimensions
    let
        actors     = (s^.player) : (s^.enemies)
        showChar   = showCharAtPos actors (s^.world) (cfg^.glyphs)
        makeLine y = [ showChar (x,y) | x <- [xmin..xmax] ] ++ newline
        textWorld  = join [ makeLine y | y <- [ymin..ymax] ]
    return $ textWorld
  where
    newline :: [IO ()]
    newline = [putChar '\n']

showCharAtPos :: [Actor] -> World -> GlyphMap -> Position -> IO ()
showCharAtPos actors w gm pos  
    | pos `inWorld` w = do
        let n = if isJust actorAtPos then
                    fromJust actorAtPos ^.name
                else
                    simplify $ w ! pos
            c = fromMaybe defaultGlyph $ gm ^? ix n
        setSGR (c^.color)
        putChar (c^.glyph)
        setSGR [Reset]
            
    | otherwise = putChar ' '
  where 
    defaultGlyph = Glyph { _glyph='!', _color=[SetColor Foreground Dull Black] }
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

