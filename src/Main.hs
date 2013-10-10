module Main where

import Rogue.Types
import Rogue.Util
import Rogue.World
import Rogue.Actions
import Rogue.Interface

import Data.Array
import System.Random
import Control.Arrow
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M

main :: IO ()
main = newStdGen >>= \g -> runRogue rogue demoConf demoState { stdGen = g }

demoConf :: RConfig
demoConf = RConfig { 
      worldSize = demoWorldSize
    , screenSize = (80, 22)
    , minRoomSize = (5,3)
    , maxRoomSize = (19,9)
    , worldGlyphs = demoGlyphs
    , bindings = demoBindings
    , tunnelThreshold = 0.75
    , roomOverlapAllowed = False
    , numTunnels = 100
    , onlyTerminalTunnels = True
    }

demoState :: RState
demoState = RState { 
      world = array ((0,0), demoWorldSize) [((x,y), Nothing) | x <- [0..10], y <- [0..10]]
    , enemies = M.empty
    , player = demoChar
    , done = False
    , stdGen = mkStdGen 0
    , floors = []
    , walls = []
    }

demoWorldSize = (200, 90)

demoBindings :: Bindings
demoBindings = [
      ('j', move S)
    , ('k', move N)
    , ('h', move W)
    , ('l', move E)
    , ('b', move SW)
    , ('n', move SE)
    , ('y', move NW)
    , ('u', move NE)
    , ('\ESC', quit)
    ]

demoChar :: Actor
demoChar = Actor { 
      hp = 5
    , maxHp = 10
    , acc = 10
    , def = 10 
    , position = (1,1)
    , name = "player"
    , glyph = '@'
    }

demoGlyphs :: WorldGlyphMap
demoGlyphs = M.fromList [
      (Floor, Glyph '.')
    , (Wall, GlyphFunc fancyGetWall)
    ]

--- assuming there is a wall at pos, get the appropriate unicode char
fancyGetWall :: Position -> World -> Char
fancyGetWall pos w = case wallDirs of
    [N] -> '╨' 
    [S] -> '╥' 
    [E] -> '╞'
    [W] -> '╡'
    [N,S] -> '║'
    [N,E] -> '╚'
    [N,W] -> '╝'
    [S,E] -> '╔'
    [S,W] -> '╗'
    [E,W] -> '═'

    [N,S,E] -> case fd of
        [W] -> '║'
        [W,NW] -> '║'
        [W,SW] -> '║'
        [W,NW,SW] -> '║'
        [NE] -> '╚'
        [SE] -> '╔'
        _ -> '╠'

    [N,S,W] -> case fd of
        [E] -> '║'
        [E,NE] -> '║'
        [E,SE] -> '║'
        [E,NE,SE] -> '║'
        [NW] -> '╝'
        [SW] -> '╗'
        _ -> '╣'
        
    [N,E,W] -> case fd of
        [S] -> '═'
        [S,SE] -> '═'
        [S,SW] -> '═'
        [S,SE,SW] -> '═'
        [NE] -> '╚'
        [NW] -> '╝'
        _ -> '╩'

    [S,E,W] -> case fd of
        [N] -> '═' 
        [N,NE] -> '═' 
        [N,NW] -> '═' 
        [N,NE,NW] -> '═' 
        [SE] -> '╔'
        [SW] -> '╗'
        _    -> '╦'

    [N,S,E,W] -> case fd of
        [NE] -> '╚'
        [NW] -> '╝'
        [SE] -> '╔'
        [SW] -> '╗'
        [NW,SW] -> '╣'
        [NE,SE] -> '╠'
        [NE,NW] -> '╩'
        [SE,SW] -> '╦'
        _    -> '╬'
  where
    checkDirs = directionsAndDirectionVectors pos
    adjacentThings = map (second (w!)) checkDirs
    wallDirs = map fst $ filter (\(d,t) -> d `elem` [N,S,E,W] && t == Just Wall) adjacentThings
    fd = map fst $ filter (\(d,t) -> t == Just Floor) adjacentThings

