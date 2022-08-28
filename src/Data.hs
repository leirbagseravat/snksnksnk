{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data where
import System.Random ( StdGen )

-- Fonte geral de dados e tipos.
--inputs do usuario
data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)

-- definicao de snake e food
type Food = (Int, Int)
type Snake = [Food]

-- default
cols :: Int
cols = 32
rows :: Int
rows = 24

data GameState = GameState      {
                   -- | Vetor que compoe Snake
                   getSnake :: Snake
,
                   -- | Posicao da fodd disponivel
                   getFood :: Food
,
                   -- | Input do usuario
                   getDirection :: Direction
,
                   -- |Status do game 
                   isGameOver :: Bool
,
                   -- |geracao aleatoria
                   getRandomStdGen :: System.Random.StdGen }