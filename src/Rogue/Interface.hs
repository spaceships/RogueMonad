module Rogue.Interface 
    (
      rogue
    ) where

import Rogue.Types
import Rogue.Util
import Rogue.World
import Rogue.Actions

import Data.Maybe (isJust, fromJust)
import Control.Monad (join, unless)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Functor
import Control.Lens hiding (Level)
import Graphics.Vty

rogue :: Rogue ()
rogue = do
    genNewWorld 
    play
    vty <- view term
    liftIO $ shutdown vty

play :: Rogue ()
play = untilQuit $ do
    vty <- view term
    updateR
    k <- liftIO $ next_event vty
    keys <- view bindings
    {-fromMaybe (alert ("no binding for key: " ++ show k) >> wait) (M.lookup k keys)-}
    fromMaybe (return ()) (M.lookup k keys)
    play

untilQuit :: Rogue () -> Rogue ()
untilQuit m = use exitGameNow >>= \d -> unless d m

-- Prints the current world
updateR :: Rogue ()
updateR = do
    vty <- view term
    world <- showWorld
    liftIO $ update vty $ pic_for_image world

showWorld :: Rogue Image
showWorld = do
    ((xmin,ymin), (xmax, ymax)) <- screenDimensions
    let makeLine y = horiz_cat <$> mapM charAtPos [(x,y) | x <- [xmin..xmax]]
    vert_cat <$> sequence [makeLine y | y <- [ymin..ymax]]

charAtPos :: Position -> Rogue Image
charAtPos pos = do
    w  <- use $ currentLevel.world
    es <- use $ currentLevel.enemies
    ss <- use $ currentLevel.seen
    vs <- use $ currentLevel.visible
    p  <- use player
    gm <- view glyphs
    let actors = p : es
        actorAtPos = actors ^? traversed.filtered (\a -> a^.position == pos)
        emptySpace = gm ^?! ix "EmptySpace"
    if S.member pos ss then return $
        let n    = if isJust actorAtPos 
                   then fromJust actorAtPos ^.name
                   else simplify $ w ^. at pos
            gly  = fromMaybe emptySpace $ gm ^? ix n
            attr = if S.member (pos) vs
                   then gly^.color 
                   else emptySpace^.color
            chr  = gly^.glyph
        in char attr chr
    else return $ char (emptySpace^.color) (emptySpace^.glyph)
  where 
    simplify Nothing                      = "Empty Space"
    simplify (Just (Floor [] Nothing))    = "Floor"
    simplify (Just (Floor (x:_) Nothing)) = show x
    simplify (Just (Floor _ (Just s)))    = show s
    simplify (Just x)                     = show x

screenDimensions :: Rogue (Position, Position)
screenDimensions = do
    p <- use (player.position)
    t <- terminal_handle
    DisplayRegion x y <- display_bounds t
    let dPos = liftP (`div` 2) (fromIntegral x, fromIntegral y)
    return (p `subP` dPos, p `addP` dPos)
