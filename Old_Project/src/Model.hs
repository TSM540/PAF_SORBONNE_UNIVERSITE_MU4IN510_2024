
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , virusX :: Int
                           , virusY :: Int
                           , speed :: Int }
  deriving (Show)


initGameState :: Int -> Int -> GameState
initGameState vx vy = makeGameState 200 300 vx vy 4

makeGameState :: Int -> Int -> Int -> Int -> Int -> GameState
makeGameState px py vx vy sp = GameState px py vx vy sp

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ _ _ sp) | px > 0 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ _ _ sp) | px < 540 = gs { persoX = px + sp }
                                 | otherwise = gs
                       
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py _ _ sp) | py > 0 = gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py _ _ sp) | py < 380 = gs { persoY = py + sp }
                                | otherwise = gs

moveTopLeft :: GameState -> GameState
moveTopLeft gs@(GameState px py _ _ sp) | px > 0 && py > 0 = gs { persoX = px - sp, persoY = py - sp }
                                | otherwise = gs

moveTopRight :: GameState -> GameState
moveTopRight gs@(GameState px py _ _ sp) | px < 540 && py > 0 = gs { persoX = px + sp, persoY = py - sp }
                                 | otherwise = gs

                              
moveBottomLeft :: GameState -> GameState
moveBottomLeft gs@(GameState px py _ _ sp) | py < 380 && px > 0 = gs { persoY = py + sp, persoX = px - sp }
                              | otherwise = gs

moveBottomRight :: GameState -> GameState
moveBottomRight gs@(GameState px py _ _ sp) | py < 380 && px < 540 = gs { persoY = py + sp, persoX = px + sp }
                                | otherwise = gs

-- déplacements avec ZQSD
gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime
  | {-K.keypressed KeycodeZ kbd && K.keypressed KeycodeQ kbd = moveTopLeft gstate
  | K.keypressed KeycodeZ kbd && K.keypressed KeycodeD kbd = moveTopRight gstate
  | K.keypressed KeycodeS kbd && K.keypressed KeycodeQ kbd = moveBottomLeft gstate 
  | K.keypressed KeycodeS kbd && K.keypressed KeycodeD kbd = moveBottomRight gstate
  | -} K.keypressed KeycodeZ kbd = moveUp gstate
  | K.keypressed KeycodeQ kbd = moveLeft gstate
  | K.keypressed KeycodeS kbd = moveDown gstate
  | K.keypressed KeycodeD kbd = moveRight gstate
  | otherwise = gstate

    
  