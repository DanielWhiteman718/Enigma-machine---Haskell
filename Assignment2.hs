{- Name: Daniel Whiteman -}
module Assignment2 where
import AssignmentHelp
import Data.List
import Data.Char

{- Types and variables -}
type Rotor = Cipher
type Reflector = [(Char, Char)]
type Offsets = [Int]
data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
-- Declaring a String constant for the alphabet
alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

{- Helper functions from Assignment 1 (**Some modified**) -}
encode :: Rotor -> Int -> Char -> Char
encode cipher offset char = cipher !! (((alphaPos char) + offset) `mod` 26)
reverseEncode :: Rotor -> Int -> Char -> Char
reverseEncode cipher offset char = alphabet !! (((fromMaybe (elemIndex char cipher)) - offset) `mod` 26)
findGuess :: Char -> [(Char, Char)] -> (Char, Char)
findGuess letter guessList | (length guessList == 0) = (letter, letter)
                           | (letter == snd (guessList !! 0)) || (letter == fst (guessList !! 0)) = guessList !! 0
                           | otherwise = findGuess letter (tail guessList)




letterSwap :: Reflector -> Char -> Char
letterSwap ref letter | ((fst (findGuess letter ref)) == letter) = (snd (findGuess letter ref))
                      | otherwise = (fst (findGuess letter ref)) 

encodeRtoL :: Char -> Enigma -> Char
encodeRtoL letter (SimpleEnigma rotL rotM rotR ref offs) = encode rotL offL (encode rotM offM (encode rotR offR letter))
                                                        where
                                                        offR = offs !! 0
                                                        offM = offs !! 1
                                                        offL = offs !! 2

encodeLtoR :: Char -> Enigma -> Char
encodeLtoR letter (SimpleEnigma rotL rotM rotR ref offs) = reverseEncode rotR offR (reverseEncode rotM offM (reverseEncode rotL offL letter))
                                                        where
                                                        offR = offs !! 0
                                                        offM = offs !! 1
                                                        offL = offs !! 2

incrementOffsets :: Offsets -> Offsets
incrementOffsets (x:xs) | (length (x:xs) == 0) = []
                        | (length (x:xs) == 1) = if (((x:xs) !! 0) == 25) then [0] else [((x:xs) !! 0 + 1)]
                        | (x == 25) = 0:(incrementOffsets xs)
                        | otherwise = (x + 1):xs
                     

rotL :: String
rotL = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
rotM :: String
rotM = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
rotR :: String
rotR = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
ref :: [(Char, Char)]
ref = [('A','Y'),('B','R'),('C','U'),('D','H'),('E','Q'),('F','S'),('G','L'),('I','P'),('J','X'),('K','N'),('M','O'),('T','Z'),('V','W')]

enigmaEncode :: Char -> Enigma -> Char
enigmaEncode letter (SimpleEnigma rotL rotM rotR ref offs) = (encodeLtoR (letterSwap ref (encodeRtoL letter sE)) sE)
                                                        where
                                                        sE = (SimpleEnigma rotL rotM rotR ref offs)
                                                  
enigmaEncodeMessage :: String -> Enigma -> String
enigmaEncodeMessage message (SimpleEnigma rotL rotM rotR ref offs) | (length message == 0) = []
                                                                   | (length message == 1) = [enigmaEncode (message !! 0) sEI] 
                                                                   | otherwise = (enigmaEncode (head message) sEI):(enigmaEncodeMessage (tail message) sEI)
                                                                  where
                                                                  sE = (SimpleEnigma rotL rotM rotR ref offs)
                                                                  sEI = (SimpleEnigma rotL rotM rotR ref (incrementOffsets offs))
                                                                