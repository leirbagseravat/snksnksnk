{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Snake where

    import Data.Map as Map ( fromList, (!) )
    
    import Data ( Direction(..), Food, Snake )
    
    --Direction vem do Data que recebe os inputs do usuario e transmite para a snake
    directionVectorMap = Map.fromList $ zip [Data.UP, Data.DOWN, Data.LEFT, Data.RIGHT]
                                            [(0, (-1)), (0, 1), ((-1), 0), (1, 0)]
    
    --Caso a snake encontre food ela atualiza o seu tamanho, caso contrario mantem
    move :: Data.Food -> Data.Direction -> Data.Snake -> (Bool, Data.Snake)
    move food direction snake = if wasFoodEaten
                                then (True, newHead : snake)
                                else (False, newHead : init snake)
        where   wasFoodEaten = newHead == food
                newHead = directionVectorMap ! direction +: head snake
                (a, b) +: (c, d) = (a + c, b + d)
    
   