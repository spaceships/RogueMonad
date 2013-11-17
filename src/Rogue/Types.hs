{-# LANGUAGE TemplateHaskell #-}

module Rogue.Types where

import Data.Array (Array)
import System.Random (Random, StdGen, Random, random, randomR)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Lens

import qualified Data.Map as M

type Rogue = StateT RState (ReaderT RConfig IO)
runRogue :: Rogue () -> RConfig -> RState -> IO ()
runRogue m c s = runReaderT (evalStateT m s) c

data RState = RState 
    { _world   :: World
    , _enemies :: M.Map Position Actor
    , _player  :: Actor
    , _done    :: Bool
    , _stdGen  :: StdGen
    , _floors  :: [Position]
    , _walls   :: [Position]
    }

data RConfig = RConfig 
    { _worldSize   :: Size
    , _minRoomSize :: Size
    , _maxRoomSize :: Size
    , _screenSize  :: Size
    , _worldGlyphs :: WorldGlyphMap
    , _bindings    :: Bindings
    , _tunnelThreshold :: Float
    , _roomOverlapAllowed :: Bool
    , _numTunnels :: Int
    , _onlyTerminalTunnels :: Bool
    }

data Actor = Actor 
    { _hp       :: Int
    , _maxHp    :: Int
    , _acc      :: Int
    , _def      :: Int
    , _position :: Position
    , _name     :: String
    , _glyph    :: Char
    }

type Bindings      = [(Char, Rogue ())]
type Position      = (Int, Int)
type Size          = (Int, Int)
type World         = Array Position Thing
type WorldGlyphMap = M.Map Thing WorldGlyph

data Thing = Floor 
           | Wall 
           | Empty
    deriving (Ord, Show, Eq)

data WorldGlyph = Glyph Char                            -- Static character
                | GlyphFunc (Position -> World -> Char) -- Dynamic character

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

makeLenses ''RState
makeLenses ''RConfig
makeLenses ''Actor

