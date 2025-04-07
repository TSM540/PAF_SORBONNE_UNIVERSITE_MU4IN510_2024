
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

-- | The virus data type
data Virus = Virus
  { x :: Int
  , y :: Int
  }
data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int ,
                             viruse :: Virus
                           }
  deriving (Show)


initGameState :: GameState
initGameState = GameState 200 300 4


moveLeft :: GameState -> GameState
moveLeft gs@(GameState x y s)  -- A MODIFIER
  | x > 0 = gs{persoX = x - s}
  | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState x y s)  -- A MODIFIER
  | x < 540 = gs{persoX = x + s}
  | otherwise = gs
                              
moveUp :: GameState -> GameState
moveUp gs@(GameState x y s)  -- A MODIFIER
  | y > 0 = gs{persoY = y - s}
  | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState x y s)  -- A MODIFIER
  | y < 380 = gs{persoY = y + s}
  | otherwise = gs

insideGameState :: Maybe (V2 Int) -> GameState -> Bool 
insideGameState coordinates gs@(GameState x y S) = 
  case coordinates of
    Just (V2 x y) -> 
          (x <= x ) && 
          (x <= (x + 100)) &&
          (y <= y) && 
          (y <= (y + 100))
    Nothing -> False

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeRight kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeUp kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeDown kbd
               then moveDown else id)

  in modif gstate
