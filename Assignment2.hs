{- Name: Daniel Whiteman -}
module Assignment2 where
import AssignmentHelp
import Data.List
import Data.Char

type Rotor = Cipher
type Reflector = [(Char, Char)]
type Offsets = [Int]
data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
-- Declaring a String constant for the alphabet
alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


encode :: Rotor -> Int -> Char -> Char
encode cipher offset char = cipher !! (((alphaPos char) - offset) `mod` 26)

reverseEncode :: Rotor -> Int -> Char -> Char
reverseEncode cipher offset char = alphabet !! (((fromMaybe (elemIndex char cipher)) + offset) `mod` 26)

findGuess :: Char -> [(Char, Char)] -> (Char, Char)
findGuess letter guessList | (length guessList == 0) = (letter, letter)
                           | (letter == snd (guessList !! 0)) = guessList !! 0
                           | otherwise = findGuess letter (tail guessList)

letterSwap :: Reflector -> Char -> Char
letterSwap ref letter = if (fst (findGuess letter ref) == letter) then snd (findGuess letter ref) else fst (findGuess letter ref)

encodeRtoL :: Char -> Enigma -> Char
encodeRtoL letter (SimpleEnigma rotL rotM rotR ref offs) = encode rotL 0 (encode rotM 0 (encode rotR 0 letter))

encodeLtoR :: Char -> Enigma -> Char
encodeLtoR letter (SimpleEnigma rotL rotM rotR ref offs) = reverseEncode rotR 0 (reverseEncode rotM 0 (reverseEncode rotL 0 letter))


{-
enigmaEncode :: Char -> Enigma -> Char
enigmaEncode letter (SimpleEnigma rotL rotM rotR ref offs) = encode rotL 0 (encode rotM 0 (encode rotR 0 letter)) -}