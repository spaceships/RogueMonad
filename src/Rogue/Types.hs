{-# LANGUAGE TemplateHaskell #-}

module Rogue.Types where

import Data.Array (Array)
import System.Random (Random, StdGen, Random, random, randomR)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Lens

import qualified Data.Map as M

type Rogue = StateT RState (ReaderT RConfig IO)
runRogue :: Rogue () -> RConfig -> RState -> IO ()
runRogue m c s = runReaderT (evalStateT m s) c

data RState = RState 
    {
      _world   :: World
    , _enemies :: [Actor]
    , _player  :: Actor
    , _stdGenR :: StdGen
    , _exitGame :: Bool
    }

data RConfig = RConfig 
    { _screenSize  :: Size
    , _bindings    :: Bindings
    , _glyphs      :: GlyphMap
    }

data Actor = Actor 
    { _hp       :: Int
    , _maxHp    :: Int
    , _acc      :: Int
    , _def      :: Int
    , _position :: Position
    , _name     :: String
    }

type Bindings = [(Char, Rogue ())]
type Position = (Int, Int)
type Size     = (Int, Int)
type World    = Array Position Thing
type Glyph    = Char
type GlyphMap = M.Map String Char

data Thing = Floor { _items :: [Item], _structure :: Maybe Structure }
           | Wall
           | EmptySpace
    deriving (Show, Eq)

emptyFloor :: Thing
emptyFloor = Floor [] Nothing

data Structure = StairsDown | StairsUp
    deriving (Show, Eq)

data Item = Item
    deriving (Show, Eq)

data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Ord, Eq, Show, Enum)

instance (Random x, Random y) => Random (x, y) where
    randomR ((x1, y1), (x2, y2)) gen1 =
        let (x, gen2) = randomR (x1, x2) gen1
            (y, gen3) = randomR (y1, y2) gen2
        in ((x, y), gen3)
    random gen1 = let (x, gen2) = random gen1
                      (y, gen3) = random gen2
                  in ((x,y), gen3)

type WorldGen = StateT WorldGenSt (Reader WorldGenCfg)

runWorldGen :: WorldGen a -> WorldGenSt -> WorldGenCfg -> a
runWorldGen m st cfg = runReader (evalStateT m st) cfg

data WorldGenSt = WorldGenSt
    { _partialWorld :: World
    , _stdGenW :: StdGen
    , _floors :: [Position]
    , _walls :: [Position]
    }

data WorldGenCfg = WorldGenCfg
    { _worldSize :: Size
    , _minRoomSize :: Size
    , _maxRoomSize :: Size
    , _tunnelThreshold :: Float
    , _roomOverlapAllowed :: Bool
    , _numTunnels :: Int
    , _onlyTerminalTunnels :: Bool
    }

makeLenses ''RState
makeLenses ''RConfig
makeLenses ''Actor
makeLenses ''Thing
makeLenses ''WorldGenSt
makeLenses ''WorldGenCfg
