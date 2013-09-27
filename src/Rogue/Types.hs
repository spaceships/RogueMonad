module Rogue.Types (
      Rogue
    , RState(..)
    , RConfig(..)
    , Character(..)
    , Position
    , Size
    , runRogue
    , module Control.Monad.State
    , module Control.Monad.Reader
    ) where

import Control.Monad.State
import Control.Monad.Reader

type Rogue = StateT RState (ReaderT RConfig IO)

runRogue :: Rogue () -> RConfig -> RState -> IO ()
runRogue m c s = runReaderT (evalStateT m s) c

-- eventually will have world information here too
data RState = RState {
      enemies :: [Character]
    , player  :: Character
    }

-- configuration options like keybindings, random seed etc.
data RConfig = RConfig {
      worldSize :: Size
    , screenSize :: Size
}

data Character = Character {
      hp  :: Int
    , maxHp :: Int
    , acc :: Int -- accuracy
    , def :: Int -- defense
    , pos :: Position
    , glyph :: Char
    }

type Position = (Int, Int)
type Size = (Int, Int)
