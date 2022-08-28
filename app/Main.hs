{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wmissing-export-lists  #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# HLINT ignore "Redundant $" #-}
module Main where


import Snake ( move )
import Food ( generateNewFood )
import World ( changeDirection, checkGameOver, initialGameState )
import Data ( Direction(DOWN, LEFT, RIGHT, UP), GameState(..) )

--
import Graphics.Gloss
    ( play,
      white,
      text,
      rectangleSolid,
      translate,
      scale,
      color,
      green,
      blue,
      black,
      pictures,
      Display(InWindow),
      Color,
      Picture, greyN )
import Graphics.Gloss.Interface.Pure.Game
    ( KeyState(Down),
      SpecialKey(KeyF1, KeyLeft, KeyRight, KeyUp, KeyDown, KeySpace),
      Key(SpecialKey),
      Event(EventKey), greyN )
import Graphics.Gloss.Data.Color (greyN)
import Debug.Trace ()
import Data.Char (intToDigit)
import Graphics.Gloss (greyN)
import Graphics.Gloss (greyN)


render :: GameState -> Graphics.Gloss.Picture
render gameState = Graphics.Gloss.pictures $   [ fillRectangle Graphics.Gloss.black (16, 0) (660, 20)
                                , fillRectangle Graphics.Gloss.black (16, 24) (660, 20)
                                , fillRectangle Graphics.Gloss.black (0, 12) (20, 480)
                                , fillRectangle Graphics.Gloss.black (32, 12) (20, 480)
                                ] ++
                                  fmap (convertToPicture Graphics.Gloss.blue) snake ++
                                  fmap (convertToPicture Graphics.Gloss.blue) [food] ++
                                  gameOverPicture
    where   snake = getSnake gameState
            food = getFood gameState
            convertToPicture :: Graphics.Gloss.Color -> (Int, Int) -> Graphics.Gloss.Picture
            convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
            fillRectangle color' (tx, ty) (w, h) =  Graphics.Gloss.color color' $
                                                    Graphics.Gloss.scale 1 (-1) $
                                                    Graphics.Gloss.translate (tx * 20 - 320) (ty * 20 - 240) $
                                                    Graphics.Gloss.rectangleSolid w h
            toFloat (x, y) = (fromIntegral x, fromIntegral y)
            gameOverPicture =   if (isGameOver gameState)
                                then [  
                                        Graphics.Gloss.color Graphics.Gloss.blue $
                                        Graphics.Gloss.translate (-200) (75) $
                                        Graphics.Gloss.scale 0.5 0.5 $
                                        Graphics.Gloss.text "Haskell Snake"
                                        ,Graphics.Gloss.color Graphics.Gloss.black $
                                        Graphics.Gloss.translate (-200) (-40) $
                                        Graphics.Gloss.scale 0.2 0.2 $
                                        Graphics.Gloss.text "-Novo Jogo"
                                        ,Graphics.Gloss.color Graphics.Gloss.black $
                                        Graphics.Gloss.translate (-200) (-80) $
                                        Graphics.Gloss.scale 0.2 0.2 $
                                        Graphics.Gloss.text "-Pressione espaco para jogar."
                                        ,Graphics.Gloss.color Graphics.Gloss.black $
                                        Graphics.Gloss.translate (-200) (-120) $
                                        Graphics.Gloss.scale 0.2 0.2 $
                                        Graphics.Gloss.text $ "-Pontuacao: " ++ show(((length$snake)-5)*10)
                                        ,Graphics.Gloss.color Graphics.Gloss.black $
                                        Graphics.Gloss.translate (-200) (-160) $
                                        Graphics.Gloss.scale 0.2 0.2 $
                                        Graphics.Gloss.text "-Em jogo, aperte F1 para Reset"
                                        ,Graphics.Gloss.color Graphics.Gloss.black $
                                        Graphics.Gloss.translate (-200) (-200) $
                                        Graphics.Gloss.scale 0.2 0.2 $
                                        Graphics.Gloss.text "-Pressione ESC para sair"
                                        ]
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
handleKeys (EventKey (SpecialKey KeyF1) Down _ _) gameState =  initialGameState False
                                                                 

handleKeys _ gameState = gameState

janela :: Graphics.Gloss.Display
janela = Graphics.Gloss.InWindow "Haskell Snake" (920, 640) (100, 100)

fundo :: Graphics.Gloss.Color
fundo = Graphics.Gloss.greyN 0x1.6666666666666p-1

main :: IO ()
main = Graphics.Gloss.play janela fundo 10 (initialGameState True) render handleKeys atualizaJogo
