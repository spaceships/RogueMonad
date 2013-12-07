#!/usr/bin/runghc

import System.Environment
import Graphics.Vty
import Data.Functor

main = do
    vty <- mkVty
    update vty $ pic_for_image $ allColors
    _ <- next_event vty
    shutdown vty

allColors :: Image
allColors = horiz_cat $ vert_cat <$> groupsOf 30 [ showColor $ Color240 x | x <- [0..239] ]

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs 
    | length xs > n = take n xs : groupsOf n (drop n xs)
    | otherwise     = [xs]

showColor :: Color -> Image
showColor c = string (Attr (SetTo bold) (SetTo c) Default) (show c ++ "  ")
