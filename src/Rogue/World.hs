module Rogue.World where

import Rogue.Types

import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow
import Control.Applicative
import Data.Array
import Data.Functor
import Data.List
import Data.Maybe
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
    x > 0 && 
    y > 0 && 
    x < maxX && 
    y < maxY
  where
    (_, (maxX, maxY)) = bounds w

inWorldR :: Position -> Rogue Bool
inWorldR p = do
    w <- gets world
    return $ p `inWorld` w
    
getFromWorld :: Position -> World -> Maybe Thing
getFromWorld p w = if p `inWorld` w then w ! p else Nothing

positionPlayer :: Rogue ()
positionPlayer = do
    p <- gets player 
    possiblePositions <- gets floors
    if length possiblePositions == 0 then
        return ()
    else do
        newPos <- randElem possiblePositions
        modify (\s -> s { 
                          player = p { position = newPos } 
                        , floors = delete newPos possiblePositions
                        }
               ) 


room :: Size -> [(Position, Maybe Thing)]
room s@(maxX, maxY) = 
    [ ((x,y), thing (x,y)) | x <- [0..maxX], y <- [0..maxY] ]
  where
    thing (x,y) | x == 0 || y == 0 || x == maxX || y == maxY = Just Wall
                | otherwise = Just Floor

blankWorld :: Rogue World
blankWorld = do
    size@(maxX, maxY) <- asks worldSize
    return $ array ((0,0), size) 
        [ ((x,y),Nothing) | y <- [0..maxY], x <- [0..maxX] ]

clearWorld :: Rogue ()
clearWorld = do
    w <- blankWorld
    modify (\s -> s { world = w })

directionVectors :: [Position -> Position]
directionVectors = [north,south,east,west]
  where 
    north = addP (0,-1)
    south = addP (0,1)
    east  = addP (1,0)
    west  = addP (-1,0)

directionToVector :: Direction -> Position -> Position
directionToVector N = directionVectors !! 0
directionToVector S = directionVectors !! 1
directionToVector E = directionVectors !! 2
directionToVector W = directionVectors !! 3

tunnelDirection :: Position -> Rogue (Maybe Direction)
tunnelDirection pos = do
    w <- gets world 

    let ds = do d <- [N,S,E,W] 
                let thing = w ! directionToVector d pos
                guard $ isNothing thing
                return d

    if length ds > 0 then do
        d <- randElem ds
        return (Just d)
    else
        return Nothing


addToWorld :: [(Position, Maybe Thing)] -> Rogue ()

addToWorld [] = return ()

addToWorld ((pos, thing):ts) = do
    w <- gets world
    when (pos `inWorld` w) $ do
        case (w ! pos) of
            Nothing -> addThing (pos, thing) w
            Just Wall -> when (thing == Just Floor) $ do
                removeWall pos
                addThing (pos,thing) w
            _ -> return ()
        addToWorld ts
  where
    addThing (p,t) w = do
        if t == Just Wall then do
            walls <- gets walls
            when (not $ p `elem` walls) $ modify (\s -> s { walls = p : walls })
        else do
            floors <- gets floors
            when (not $ p `elem` floors) $ modify (\s -> s { floors = p : floors })
        modify (\s -> s { world = w // [(p,t)] })
    removeWall p = do
        walls <- gets walls
        modify (\s -> s { walls = delete p walls })
        

fitsInWorld :: [(Position, Maybe Thing)] -> Rogue Bool
fitsInWorld thgs = do
    w <- gets world
    if not $ all (`inWorld` w) (fst <$> thgs) then
        return False
    else do
        overlapAllowed <- asks overlapAllowed
        if overlapAllowed then
            return True
        else do
            -- check all elements for overlap
            return $ any isJust $ (w !) <$> (fst <$> thgs)
            
        

createRoom :: Direction -> Position -> Rogue Bool
createRoom dir pos@(px,py) = roomLoop 0 
  where 
    roomLoop n = if (n >= 10) then return False else do
        min  <- asks minRoomSize
        max  <- asks maxRoomSize
        size@(rx,ry) <- randR (min, max)
        let r = room size
            
        r' <- case dir of
            N -> do n <- randR (1,rx-1)
                    return $ fmap (first (addP (px - n, py - ry))) r
            S -> do n <- randR (1,rx-1)
                    return $ fmap (first (addP (px - n, py))) r
            E -> do n <- randR (1,ry-1)
                    return $ fmap (first (addP (px, py - n))) r
            W -> do n <- randR (1,ry-1)
                    return $ fmap (first (addP (px - rx, py - n))) r

        fits <- fitsInWorld r'
        if fits then do
            addToWorld r'
            return True
        else do
            roomLoop (n+1)        

createWorld :: Rogue ()
createWorld = do
    clearWorld
    makeInitialRoom
    tunnelLoop 0
  where
    tunnelLoop n = when (n < 100) $ do
        walls <- gets walls
        when (length walls > 0) $ do 
            wallsWithAdjacentFloors <- filterM adjacentFloor walls
            wall <- randElem wallsWithAdjacentFloors
            dir <- tunnelDirection wall
            when (isJust dir) (tunnel wall $ fromJust dir)
            tunnelLoop (n + 1)

adjacentFloor :: Position -> Rogue Bool
adjacentFloor pos = do
    w <- gets world
    return $ any (== Just Floor) $ fmap (w !) (directionVectors <*> pure pos)

tunnel :: Position -> Direction -> Rogue ()
tunnel pos dir = do

    let newThings = do
            x <- [-1,0,1]
            y <- [-1,0,1]
            guard $ x /= 0 && y /= 0
            return (pos `addP` (x,y), Just Wall)

    addToWorld $ (pos, Just Floor) : newThings
    k <- asks tunnelThreshold
    n <- randR (0,10)

    if n > k * 10 then do
        ok <- createRoom dir pos
        when (not ok) endTunnel

    else if n < k * 10 then do
        let newPos = (directionToVector dir pos)
        ok <- inWorldR newPos
        if ok then
            tunnel newPos dir
        else
            endTunnel
            

    else do -- n == k
        newDir <- newDirection
        tunnel pos newDir

  where 
    newDirection = randElem (if dir `elem` [N,S] then [E,W] else [N,S])
    endTunnel = addToWorld [(pos, Just Wall)]

makeInitialRoom :: Rogue ()
makeInitialRoom = do
    dir <- randElem [N,S,E,W]
    maxSize <- asks maxRoomSize
    worldSize <- asks worldSize
    pos <- randR ((0,0), worldSize `subP` maxSize) 
    _ <- createRoom dir pos
    return ()

step :: Rogue ()
step = do
    printR "step>"
    c <- liftIO getChar
    when (c == '\ESC') (error "execution aborted")

