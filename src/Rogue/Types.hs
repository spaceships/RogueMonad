{-# LANGUAGE TemplateHaskell #-}

module Rogue.Types where

import Data.Array (Array)
import System.Random (Random, StdGen, Random, random, randomR)
import System.Console.ANSI (SGR)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as S

type Rogue = StateT RState (ReaderT RConfig IO)
runRogue :: Rogue () -> RConfig -> RState -> IO ()
runRogue m c s = runReaderT (evalStateT m s) c

data RState = RState 
    {
      _world   :: World
    , _seen    :: S.Set Position
    , _visible :: S.Set Position
    , _enemies :: [Actor]
    , _player  :: Actor
    , _stdGenR :: StdGen
    , _exitGame :: Bool
    }

data RConfig = RConfig 
    { _screenSize :: Size
    , _bindings   :: Bindings
    , _glyphs     :: GlyphMap
    , _viewRadius :: Int
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
type GlyphMap = M.Map String Glyph

data Glyph = Glyph { _glyph :: Char, _color :: [SGR] }

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

makeLenses ''RState
makeLenses ''RConfig
makeLenses ''Actor
makeLenses ''Thing
makeLenses ''Glyph
