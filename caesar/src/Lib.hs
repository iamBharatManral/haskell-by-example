module Lib (caesar, rot13) where

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

isUpper :: Char -> Bool
isUpper ch = ch `elem` upperAlphabet

isLower :: Char -> Bool
isLower ch = ch `elem` lowerAlphabet

indexOf :: Char -> Alphabet -> Int
indexOf _ [] = undefined
indexOf ch (x : xs) = if x == ch then 0 else 1 + indexOf ch xs

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alp n ch = alp !! ((indexOf ch alp + n) `mod` length alp)

upperRot :: Int -> Char -> Char
upperRot n ch = alphabetRot upperAlphabet n ch

lowerRot :: Int -> Char -> Char
lowerRot n ch = alphabetRot lowerAlphabet n ch

rotChar :: Int -> Char -> Char
rotChar n ch
  | isLower ch = lowerRot n ch
  | isUpper ch = upperRot n ch
  | otherwise = ch

caesar :: Int -> String -> String
caesar _ [] = []
caesar n message = map (\ch -> rotChar n ch) message

rot13 :: String -> String
rot13 message = caesar 13 message
