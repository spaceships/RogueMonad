{-# LANGUAGE TemplateHaskell #-}

module Rogue.Types where

import System.Random (Random, StdGen, Random, random, randomR)
import Control.Lens (makeLenses)
import Graphics.Vty (Attr, Vty, Event)
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

type Rogue = StateT RState (ReaderT RConfig IO)
runRogue :: Rogue () -> RConfig -> RState -> IO ()
runRogue m c s = runReaderT (evalStateT m s) c

data RState = RState 
    { _player       :: Actor
    , _stdGenR      :: StdGen
    , _exitGameNow  :: Bool
    , _depth        :: Int
    , _currentLevel :: Level
    , _upperLevels  :: [Level]
    , _lowerLevels  :: [Level]
    }

data RConfig = RConfig 
    { _bindings   :: Bindings
    , _glyphs     :: GlyphMap
    , _viewRadius :: Int
    , _term       :: Vty
    , _numLevels  :: Int
    , _numStairs  :: Int
    }

data Level = Level
    { _seen       :: S.Set Position -- Positions on this level seen already
    , _visible    :: S.Set Position -- Positions visible from player's current spot
    , _enemies    :: [Actor]
    , _world      :: World
    , _stairsDown :: M.Map Position Position
    , _stairsUp   :: M.Map Position Position
    }

data Actor = Actor 
    { _hp       :: Int
    , _maxHp    :: Int
    , _acc      :: Int
    , _def      :: Int
    , _position :: Position
    , _name     :: String
    }

data Glyph = Glyph { _glyph :: Char, _color :: Attr }

data Thing = Floor { _items :: [Item], _structure :: Maybe Structure }
           | Wall
           | EmptySpace
    deriving (Show, Eq)

data Structure = StairsDown
               | StairsUp
    deriving (Show, Eq)

data Item = Item
    deriving (Show, Eq)

data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Ord, Eq, Show, Enum)

type World    = A.Array Position Thing
type Bindings = M.Map Event (Rogue ())
type GlyphMap = M.Map String Glyph
type Position = (Int, Int)
type Size     = (Int, Int)

instance (Random x, Random y) => Random (x, y) where
    randomR ((x1, y1), (x2, y2)) gen1 =
        let (x, gen2) = randomR (x1, x2) gen1
            (y, gen3) = randomR (y1, y2) gen2
        in ((x, y), gen3)
    random gen1 = let (x, gen2) = random gen1
                      (y, gen3) = random gen2
                  in ((x,y), gen3)

emptyLevel :: Level
emptyLevel = Level
    { _seen = S.empty
    , _visible = S.empty
    , _enemies = []
    , _world = emptyWorld
    , _stairsUp = M.empty
    , _stairsDown = M.empty
    }

emptyWorld :: World
emptyWorld = A.array ((0,0),(0,0)) [((0,0),EmptySpace)]

emptyFloor :: Thing
emptyFloor = Floor [] Nothing

makeLenses ''RState
makeLenses ''RConfig
makeLenses ''Level
makeLenses ''Actor
makeLenses ''Thing
makeLenses ''Glyph
