{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Food where

import System.Random ( Random(randomR), StdGen )

import Data ( Food, Snake )


generateNewFood :: Data.Snake -- ^ 
  -> StdGen -- ^ 
  -> (Data.Food, StdGen)
generateNewFood snake stdGen =  if newFood `elem` snake
                                then generateNewFood snake stdGen3 
                                else ((foodX, foodY), stdGen3)
        where   (foodX, stdGen2) = randomR (1, 31) stdGen
                (foodY, stdGen3) = randomR (1, 23) stdGen2
                newFood = (foodX, foodY)