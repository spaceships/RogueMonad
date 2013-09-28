module Rogue.Types (
      Rogue
    , RState(..)
    , RConfig(..)
    , Actor(..)
    , WorldGlyphMap
    , Position
    , Size
    , World
    , Thing(..)

    , runRogue
    , ask
    , asks
    , get
    , gets
    , liftIO
    ) where

import Control.Monad.State
import Control.Monad.Reader
import Data.Array
import qualified Data.Map as Map

type Rogue = StateT RState (ReaderT RConfig IO)

runRogue :: Rogue () -> RConfig -> RState -> IO ()
runRogue m c s = runReaderT (evalStateT m s) c

data RState = RState {
      world   :: World
    , enemies :: [Actor]
    , player  :: Actor
    }

data RConfig = RConfig {
      worldSize :: Size
    , screenSize :: Size
    , worldGlyphs :: WorldGlyphMap
}

data Actor = Actor {
      hp  :: Int
    , maxHp :: Int
    , acc :: Int
    , def :: Int
    , pos :: Position
    , name :: String
    , glyph :: Char
    }

type Position = (Int, Int)

type Size = (Int, Int)

type World = Array (Int, Int) (Maybe Thing)

data Thing = Floor | Wall
    deriving (Ord, Show, Eq)

type WorldGlyphMap = Map.Map Thing Char
