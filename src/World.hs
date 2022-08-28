{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wmissing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module World where

import Data.Map as Map ()
import System.Random ( mkStdGen )

import Data ( cols, rows, Direction(DOWN), GameState(..), Snake )

checkGameOver :: Data.Snake -- recebe Data.snake e retorna um status do game (True/False), verificando se a posicao da cabeca colidiu com colunas, rows ou com a tail^ 
  -> Bool
checkGameOver snake =   headX == 0 || headX == Data.cols ||
                        headY == 0 || headY == Data.rows ||
                        head' `elem` tail'
    where   head' = head snake
            (headX, headY) = head'
            tail' = tail snake


initialGameState :: Bool -> GameState
initialGameState gameOver = Data.GameState   { getSnake = [  (snakeX, snakeY),
                                                        (snakeX, snakeY - 1),
                                                        (snakeX, snakeY - 2),
                                                        (snakeX - 1, snakeY - 2),
                                                        (snakeX - 2, snakeY - 2)]
                                        , getFood = (3, 3)
                                        , getDirection = Data.DOWN
                                        , isGameOver = gameOver
                                        , getRandomStdGen = System.Random.mkStdGen 100 }
        where   snakeX = Data.cols `div` 2
                snakeY = Data.rows `div` 2

changeDirection :: Data.GameState -- verificar o status do game para leitura do input de direcao, atualiza recusirvamenge o game state^ 
  -> Data.Direction -- caso o game esteja ativo recebe a direcao do usuario^ 
  -> Data.GameState
changeDirection (Data.GameState s f d g r) newDir = Data.GameState s f newDir g r
    