module Rogue.World where

import Rogue.Types

import Control.Monad.State
import Control.Monad.Reader
import Data.Array
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F
import qualified Data.Map as M

showWorld :: Rogue String
showWorld = do
    st  <- get
    cfg <- ask
    
    ((xmin,ymin), (xmax, ymax)) <- screenDimensions

    let
        p                = player st
        actors           = M.insert (position p) p (enemies st)
        getChar          = getCharAtPos actors (world st) (worldGlyphs cfg)
        makeLine y       = [ getChar (x,y) | x <- [xmin..xmax] ]
        textWorld cfg st = unlines [ makeLine y | y <- [ymin..ymax] ]

    return $ textWorld cfg st

        
getCharAtPos :: M.Map Position Actor -> World -> WorldGlyphMap -> Position -> Char
getCharAtPos things w gm pos = 
    if pos `inWorld` w then 
        maybe (toGlyph gm $ w ! pos) glyph $ M.lookup pos things 
    else
        ' ' 

screenDimensions :: Rogue (Position, Position)
screenDimensions = do
    p <- gets (position . player)
    size <- asks screenSize
    let dPos = liftP (`div` 2) size
    return (p `subP` dPos, p `addP` dPos)


toGlyph :: WorldGlyphMap -> Maybe Thing -> Char
toGlyph gm (Just x) = fromMaybe ' ' $ M.lookup x gm
toGlyph _ Nothing   = ' '

inWorld :: Position -> Array Position a -> Bool
inWorld (x,y) w = 
    x >= 0 && 
    y >= 0 && 
    x <= maxX && 
    y <= maxY
  where
    (_, (maxX, maxY)) = bounds w

inWorldR :: Position -> Rogue Bool
inWorldR p = do
    w <- gets world
    return $ p `inWorld` w
    
room :: Size -> World
room s@(maxX, maxY) = 
    array ((0,0), s) [ ((x,y), thing (x,y)) | x <- [0..maxX], y <- [0..maxY] ]
  where
    thing (x,y) | x == 0 || y == 0 || x == maxX || y == maxY = Just Wall
                | otherwise = Just Floor

createWorld :: Rogue ()
createWorld = do
    nRooms <- asks maxRooms
    rooms <- replicateM nRooms createRoom
    w <- blankWorld 
    w' <- F.foldrM assignRoom w rooms
    positionPlayer 
    modify (\s -> s { world = w' })

positionPlayer :: Rogue ()
positionPlayer = do
    p <- gets player 
    possiblePositions <- gets emptyFloors
    n <- randR (0, length possiblePositions - 1)
    let newPos = possiblePositions !! n
        possiblePositions' = delete newPos possiblePositions
    modify (\s -> s { 
          player = p { position = newPos } 
        , emptyFloors = possiblePositions'
        }) 


createRoom :: Rogue World
createRoom = do
    min <- asks minRoomSize
    max <- asks maxRoomSize
    size <- randR (min, max)
    return $ room size

blankWorld :: Rogue World
blankWorld = do
    size@(maxX, maxY) <- asks worldSize
    return $ array ((0,0), size) 
        [ ((x,y),Nothing) | y <- [0..maxY], x <- [0..maxX] ]

assignRoom :: World -> World -> Rogue World
assignRoom room w = loop 0
  where
    modifyIndices f = fmap (\(i,x) -> (f i, x))
    isFloor p       = w ! p == Just Floor
    (_, roomSize)   = bounds room
    loop try | try >= 100 = return w
             | otherwise  = do
        worldSize         <- asks worldSize
        pMin@(xMin, yMin) <- randR ((0,0), worldSize `subP` roomSize)
        let pMax@(xMax, yMax) = pMin `addP` roomSize
        if all (not . isFloor) [pMin, pMax, (xMin, yMax), (xMax, yMin)] then do
            let room' = modifyIndices (addP pMin) $ assocs room
            forM_ room' $ \(p,t) -> when (t == Just Floor) $ do
                floors <- gets emptyFloors
                modify (\s -> s { emptyFloors = p : floors })
            return $ w // room'
        else
            loop (try + 1)
