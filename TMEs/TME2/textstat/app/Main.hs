{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Stats
import System.IO


main :: IO ()
main = do
    -- contents <- readFile "test.txt"
    -- putStrLn contents
  fichier <- TIO.readFile "test.txt" 
  TIO.putStrLn fichier
  putStrLn $ "Analyse du texte de: test.txt " 
  TIO.putStrLn "===================="
  nbCaracteres <- text_length fichier
  putStrLn $ "Nombre de caractères: " ++ show nbCaracteres
  -- nbMots <- count_number_of_words fichier
  -- TIO.putStrLn $ "Nombre de mots: " ++ show nbMots
  -- TIO.putStrLn "*** Caracteres de " ++ T.unpack fichier ++ "***"
  -- caracteres <- foldl (\acc c -> acc ++ [c]) [] ['a' .. 'z']
  -- mapM_ (\c -> do
  --       nb <- count_number_of_specic_char fichier c
  --       TIO.putStrLn $ "Nombre de " ++ show c ++ ": " ++ show nb
  --       ) caracteres



