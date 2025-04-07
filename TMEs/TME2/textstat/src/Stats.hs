module Stats(
    text_length, 
    count_number_of_words , 
    count_number_of_specic_char
) where 
import Data.Text (Text)
import qualified Data.Text as T

text_length :: Text -> Int 
text_length t = T.length t


-- on n'a pas utilisé fold l car on aura un soucis quand y'aura deux blancs successifs 
count_number_of_words :: Text -> Int 
count_number_of_words t =  length(T.split (==' ') t)



count_number_of_specic_char :: Text -> Char -> Int 
count_number_of_specic_char t c =  T.foldl (\acc x -> if x == c then acc + 1 else acc) 0 t