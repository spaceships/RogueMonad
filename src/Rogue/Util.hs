module Rogue.Util where

import Rogue.Types

import Data.List (unfoldr)
import System.Random (Random, random, randomR)
import System.Console.ANSI (clearScreen, setCursorPosition)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as S
import Control.Lens

-- modified takeWhile that also takes the first elem to fail p
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else [x]
takeWhile' _ [] = []

segment :: Position -> Position -> Int -> [Position]
segment p1 p2 r = takeWhile lessThanR $ line p1 p2
  where
    lessThanR p3 = distance p1 p3 < fromIntegral r

-- Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
line :: Position -> Position -> [Position]
line p1@(x0, y0) (x1, y1) =
    let (dx, dy) = (x1 - x0, y1 - y0)
        xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
        yxStep b (x, y) = (x + signum dx * b, y + signum dy)
        (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                     | otherwise       = (abs dx, abs dy, yxStep)
        walk w xy = xy : walk (tail w) (step (head w) xy)
    in  walk (balancedWord p q 0) (x0, y0)
  where
    balancedWord :: Int -> Int -> Int -> [Int]
    balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
    balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)

circlePoints' :: Position -> Int -> [Position]
circlePoints' p r = concatMap (circlePoints p) [r+2,r+1,r] -- [r,r-1..r-5]

-- get the points corresponding to a circle of radius r around position p
circlePoints :: Position -> Int -> [Position]
circlePoints p@(x0,y0) r =
    [ (x0,y0+r)
    , (x0,y0-r)
    , (x0+r,y0)
    , (x0-r,y0)
    , p `addP` (x,x)
    , p `addP` (x,-x)
    , p `addP` (-x,-x)
    , p `addP` (-x,x)
    ] ++ points
  where
    x = round $ sqrt ( (fromIntegral r)^2 / 2 )
    points = concatMap generatePoints $ unfoldr step initialValues
    generatePoints (x,y) = do
        (x', y') <- [(x,y),(y,x)]
        xop <- [(+),(-)]
        yop <- [(+),(-)]
        return (xop x0 x',yop y0 y')
    initialValues = (1 - r, 1, (-2) * r, 0, r)
    step (f, ddf_x, ddf_y, x, y)
        | x >= y    = Nothing
        | otherwise = Just ((x', y'), (f', ddf_x', ddf_y', x', y'))
      where
        (f', ddf_y', y')
            | f >= 0    = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
            | otherwise = (f + ddf_x, ddf_y, y)
        ddf_x' = ddf_x + 2
        x'     = x + 1

isFloor :: Thing -> Bool
isFloor (Floor _ _) = True
isFloor _ = False

liftP :: (a -> b) -> (a, a) -> (b, b)
liftP f (a, b) = (f a, f b)

liftP2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
liftP2 f (ax, ay) (bx, by) = (f ax bx, f ay by)

addP :: Position -> Position -> Position
addP = liftP2 (+)

subP :: Position -> Position -> Position
subP = liftP2 (-)

distance :: Floating a => Position -> Position -> a
distance (x1,y1) (x2,y2) = sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)

rand :: Random a => Rogue a
rand = do
    g <- use stdGenR
    let (a, g') = random g
    stdGenR .= g'
    return a

randR :: Random a => (a,a) -> Rogue a
randR range = do
    g <- use stdGenR
    let (a, g') = randomR range g
    stdGenR .= g'
    return a

randElemR :: [a] -> Rogue a
randElemR xs = do
    n <- randR (0, length xs - 1)
    return (xs !! n)

printR :: Show a => a -> Rogue ()
printR = liftIO . print

center :: String -> Int -> String
center s w = replicate left ' ' ++ s ++ replicate right ' '
  where
    l = length s
    toBeFilled = w - l
    (both, leftAdd) = toBeFilled `divMod` 2
    (left, right) = (both + leftAdd, both)

progressBar :: String -> Int -> Rogue (Int -> Rogue ())
progressBar label total = do
    liftIO clearScreen
    (maxX,maxY) <- view screenSize
    let length = 20
        y = maxY `div` 2
    liftIO $ setCursorPosition (y-1) 0
    liftIO $ putStrLn (center label maxX)
    return $ \n -> do
        liftIO $ setCursorPosition y 0
        let p = floor ((fromIntegral n / fromIntegral total)
                        * fromIntegral length)
            s = "|" ++ replicate p '=' ++ replicate (length - p) ' ' ++ "|"
        liftIO $ putStr $ center s maxX
