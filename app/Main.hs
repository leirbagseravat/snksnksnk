module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

render :: GameState -> Picture
render gameState = pictures $   [ fillRectangle black (16, 0) (640, 20)
                                , fillRectangle black (16, 24) (640, 20)
                                , fillRectangle black (0, 12) (20, 480)
                                , fillRectangle black (32, 12) (20, 480) ] ++
                                  fmap (convertToPicture black) snake ++
                                  fmap (convertToPicture green) [food] ++
                                  gameOverPicture
    where   snake = getSnake gameState
            food = getFood gameState
            convertToPicture :: Color -> (Int, Int) -> Picture
            convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
            fillRectangle color' (tx, ty) (w, h) =  color color' $
                                                    scale 1 (-1) $
                                                    translate (tx * 20 - 320) (ty * 20 - 240) $
                                                    rectangleSolid w h
            toFloat (x, y) = (fromIntegral x, fromIntegral y)
            gameOverPicture =   if (isGameOver gameState)
                                then [  color green $
                                        translate (-200) (0) $
                                        scale 0.5 0.5 $
                                        text "FIM DE JOGO"
                                     ,  color green $
                                        translate (-175) (-50) $
                                        scale 0.15 0.15 $
                                        text "Pressione espaco para tentar novamente." ]
                                else []

atualizaJogo :: Float -> GameState -> GameState
atualizaJogo segundos estado =  if (gameOver)
                            then estado
                            else GameState newSnake newFood' direction newGameOver newStdGen
    where   snake = getSnake estado
            food = getFood estado
            direction = getDirection estado
            gameOver = isGameOver estado
            stdGen = getRandomStdGen estado
            (wasFoodEaten, newSnake) = move food direction snake
            (newFood, newStdGen) = generateNewFood newSnake stdGen
            newFood' =  if wasFoodEaten
                        then newFood
                        else food
            newGameOver = checkGameOver newSnake

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = changeDirection gameState LEFT
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = changeDirection gameState RIGHT
handleKeys (EventKey (SpecialKey KeyUp   ) Down _ _) gameState = changeDirection gameState UP
handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) gameState = changeDirection gameState DOWN
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState =    if (isGameOver gameState)
                                                                    then initialGameState False
                                                                    else gameState
handleKeys _ gameState = gameState

janela :: Display
janela = InWindow "Haskell Snake Game" (920, 640) (100, 100)

fundo :: Color
fundo = white

main :: IO ()
main = play janela fundo 10 (initialGameState True) render handleKeys atualizaJogo
