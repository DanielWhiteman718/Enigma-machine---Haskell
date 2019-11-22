{- Name: Daniel Whiteman -}
module Assignment2 where
import AssignmentHelp
import Data.List
import Data.Char

{- Types and variables -}
type Rotor = Cipher
type Reflector = [(Char, Char)]
type Offsets = [Int]
type Steckerboard = [(Char, Char)]
data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets 
            | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Steckerboard
type Crib = (String, String)
type Menu = [Int]

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


rotorEncode :: Rotor -> Int -> Char -> Char
rotorEncode rotor offset letter = alphabet !! (((alphaPos(encode rotor 0 (alphabet !! (((alphaPos letter) + offset) `mod` 26)))) - offset) `mod` 26)

reverseRotorEncode :: Rotor -> Int -> Char -> Char 
reverseRotorEncode rotor offset letter = alphabet !! ((alphaPos (reverseEncode rotor 0 (alphabet !! (((alphaPos letter) + offset) `mod` 26))) - offset)`mod` 26)

letterSwap :: Reflector -> Char -> Char
letterSwap ref letter | ((fst (findGuess letter ref)) == letter) = (snd (findGuess letter ref))
                      | otherwise = (fst (findGuess letter ref))  

steckerLetterSwap :: Steckerboard -> Char -> Char
steckerLetterSwap steckerboard letter | ((fst (findGuess letter steckerboard)) == letter) = (snd (findGuess letter steckerboard))
                                      | otherwise = (fst (findGuess letter steckerboard)) 

encodeRtoL :: Char -> Enigma -> Char
encodeRtoL letter (SimpleEnigma rotL rotM rotR ref offs) = rotorEncode rotL (offs !! 2) (rotorEncode rotM (offs !! 1) (rotorEncode rotR (offs !! 0) letter))

encodeLtoR :: Char -> Enigma -> Char
encodeLtoR letter (SimpleEnigma rotL rotM rotR ref offs) = reverseRotorEncode rotR (offs !! 0) (reverseRotorEncode rotM (offs !! 1) (reverseRotorEncode rotL (offs !! 2) letter))
                                                        

incrementOffsets :: Offsets -> Offsets
incrementOffsets (x:xs) | (length (x:xs) == 0) = []
                        | (length (x:xs) == 1) = if (((x:xs) !! 0) == 25) then [0] else [((x:xs) !! 0 + 1)]
                        | (x == 25) = 0:(incrementOffsets xs)
                        | otherwise = (x + 1):xs
                     
steckerLetter :: Steckerboard -> Char -> Char
steckerLetter steckerboard letter | ((fst (findGuess letter steckerboard)) == letter) = (snd (findGuess letter steckerboard))
                                  | otherwise = (fst (findGuess letter steckerboard))  









{------"ABCDEFGHIJKLMNOPQRSTUVWXYZ"-}
testMessage :: String
testMessage = "INXTHEXENIGMAXMACHINEXEACHXROTORXHADXAXNOTCHSTOPXINXTHEXSPECIFICATIONCOMMAXIXHAVEXASSUMEDXTHATXTHATXNOTCHXISXALWAYSXATXPOSITTIONXTWENTYFIVEXZXXWHEREASXINXREALITYXITXWASXATXAXDIFFERENTXPOSITIONXFORXEACHXROTORSTOPXWHENXAXKEYXWASXPRESSEDCOMMAXTHEXVERYXFIRSTXTHINGXTHATXHAPPENEDXWASXTHATXTHEXROTORSXWEREXADVANCEDSTOPXTHEXRIGHTXROTORXISXROTATEDXBYXONESTOPXIFXITXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMESXTHATXISXWASXATXPOSITIONXTWENTYFIVECOMMAXTHEXNOTCHXWOULDXCAUSEXTHEXMIDDLEXROTORXTOXALSOXROTATESTOPXIFXTHATXROTORXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMECOMMAXITXINXTURNXWOULDXCAUSEXTHEXLEFTXROTORXTOXROTATESTOPXINXOTHERXWORDSCOMMAXFORXTHEXMIDDLEXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXKEYXPRESSESSTOPXFORXTHEXLEFTXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSESSTOPXTOXGETXALLXTHEXWAYXBACKXTOXZEROCOMMAZEROCOMMAZEROCOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSEESSTOPTHEXDOWNSIDEXOFXTHEXSIMPLIFICATIONXTHATXIXHAVEXGIVENXISXTHATXTHEXONLINEXSIMULATORSXWILLXNOTXPROGRESSXTHEXSUBSEQUENTXROTORSXATXTHEXSAMEXTIMEXTHATXYOURXSIMULATIONXDOESSTOPXINXACTUALXFACTXROTORXONEXHADXAXNOTCHXATXPOSITIONXQCOMMAXROTORTWOXATXPOSITIONXECOMMAXROTORTHREEXATXPOSITIONXVCOMMAXROTORFOURXATXPOSITIONXJCOMMAXANDXROTORFIVEXATXPOSITIONXZSTOP"


enigmaEncode :: Char -> Enigma -> Char
enigmaEncode letter (SimpleEnigma rotL rotM rotR ref offs) = (encodeLtoR (letterSwap ref (encodeRtoL letter sE)) sE)
                                                        where
                                                        sE = (SimpleEnigma rotL rotM rotR ref offs)
enigmaEncode letter (SteckeredEnigma rotL rotM rotR ref offs sb) = steckerLetter sb (encodeLtoR (letterSwap ref (encodeRtoL (steckerLetter sb letter) sE)) sE)
                                                        where
                                                        sE = (SimpleEnigma rotL rotM rotR ref offs)
                                                        sTE = (SteckeredEnigma rotL rotM rotR ref offs sb)
                                                  
enigmaEncodeMessage :: String -> Enigma -> String
enigmaEncodeMessage message (SimpleEnigma rotL rotM rotR ref offs) | (length message == 0) = []
                                                                   | (length message == 1) = [enigmaEncode (message !! 0) (SimpleEnigma rotL rotM rotR ref (incrementOffsets offs))] 
                                                                   | otherwise = (enigmaEncode (head message) (SimpleEnigma rotL rotM rotR ref (incrementOffsets offs))):(enigmaEncodeMessage (tail message) (SimpleEnigma rotL rotM rotR ref (incrementOffsets offs)))
enigmaEncodeMessage message (SteckeredEnigma rotL rotM rotR ref offs sb) | (length message == 0) = []
                                                                         | (length message == 1) = [enigmaEncode (message !! 0) (SteckeredEnigma rotL rotM rotR ref (incrementOffsets offs) sb)] 
                                                                         | otherwise = (enigmaEncode (head message) (SteckeredEnigma rotL rotM rotR ref (incrementOffsets offs) sb)):(enigmaEncodeMessage (tail message) (SteckeredEnigma rotL rotM rotR ref (incrementOffsets offs) sb))


{- findIndexes 'I' 0 "Increment" [0] -}                                                                         
findIndexes :: Char -> Int -> String -> [Int] -> [Int]
findIndexes letter cidx string indexes | ((length string) == 0) = []
                                       | ((length string) == 1) = if ((string !! 0) == letter) then (tail (indexes ++ [cidx]))
                                                                  else tail indexes
                                       | ((head string) == letter) = findIndexes letter (cidx + 1) (tail string) (indexes ++ [cidx])
                                       | otherwise = findIndexes letter (cidx + 1) (tail string) indexes

findOpposite :: Crib -> Int -> Char
findOpposite crib index = (snd crib) !! index

{-Helpful data-}
crib1 :: Crib
crib1 = ("WETTERVORHERSAGEBISKAYA","RWIVTYRESXBFOGKUHQBAISE")

crib2 :: Crib
crib2 = ("ABC","ZAY")

crib3 :: Crib
crib3 = ("WETTER","RWIVTY")
{- findSuccessors [5,6,3,6] "Increment" crib-}     
{- findSuccessors [0] crib1 -}                                 
findSuccessors :: [Int] -> Crib -> [[Int]]
findSuccessors node crib | (length node == 0) = [[]]
                         | (length node == 1) = [(findIndexes (findOpposite crib (head node)) 0 (fst crib) [0])]
                         | otherwise = (findIndexes (findOpposite crib (head node)) 0 (fst crib) [0]):(findSuccessors (tail node) crib)
    
{-type Menu = [Int]-}                         

findMenus :: [Int] -> Crib -> Menu -> [Menu]
findMenus node crib menu | ((length node) == 0) = [menu]
                         | ((length node) == 1) && (elem (head node) menu) = [menu]
                         | ((length node) == 1) && (not(elem (head node) menu)) = (findMenus (head (findSuccessors [head node] crib)) crib ((head node):menu))
                         | (elem (head node) menu) = (findMenus (tail node) crib menu)                                                           
                         | otherwise = ( findMenus (head (findSuccessors [head node] crib)) crib ((head node):menu) ) ++ (findMenus (tail node) crib menu)

findLongestMenu :: [Menu] -> Menu
findLongestMenu menus = head (sortBy (\xs ys -> compare (length ys) (length xs)) menus)

findLongestMenuEachIndex :: Int -> Crib -> [Menu]
findLongestMenuEachIndex index crib | (index == length(fst crib)) = []
                                    | (index == length(fst crib) - 1) = [findLongestMenu (findMenus [index] crib [])]
                                    | otherwise = (findLongestMenu (findMenus [index] crib [])):(findLongestMenuEachIndex (index + 1) crib)

longestMenu :: Crib -> Menu
longestMenu crib = reverse(findLongestMenu(findLongestMenuEachIndex 0 crib))