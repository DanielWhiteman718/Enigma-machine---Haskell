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





{- rotorEncode(Helper function). Takes a rotor, an offset
   and a character. Returns the encoded character. 
   r = Rotor, o = Offset, l = Letter being encoded-}

rotorEncode :: Rotor -> Int -> Char -> Char
rotorEncode r o l = alphabet !! (((alphaPos(encode r 0 (alphabet !! (((alphaPos l) + o) `mod` 26)))) - o) `mod` 26)


{- reverseRotorEncode(Helper function). Takes a rotor, an offset
   and a character. Returns the decoded character. 
   r = Rotor, o = Offset, l = Letter being decoded-}

reverseRotorEncode :: Rotor -> Int -> Char -> Char 
reverseRotorEncode r o l = alphabet !! ((alphaPos (reverseEncode r 0 (alphabet !! (((alphaPos l) + o) `mod` 26))) - o)`mod` 26)


{- letterSwap(Helper function). Takes a reflector and a letter.
   Returns the letter that comes out of the reflector.
   r = Reflector, l = Letter to be swapped-}

letterSwap :: Reflector -> Char -> Char
letterSwap r l | ((fst (findGuess l r)) == l) = (snd (findGuess l r))
               | otherwise = (fst (findGuess l r))  


{- encodeRtoL(Helper function). Takes a letter and enigma.
   Returns a letter after putting l throught the right, middle
   and left rotors (in that order).
   l = Letter to be encoded-}

encodeRtoL :: Char -> Enigma -> Char
encodeRtoL l (SimpleEnigma rL rM rR rf os) = rotorEncode rL (os !! 2) (rotorEncode rM (os !! 1) (rotorEncode rR (os !! 0) l))


{- encodeLtoR(Helper function). Takes a letter and enigma.
   Returns a letter after putting l throught the left, middle
   and right rotors (in that order).
   l = Letter to be encoded-}

encodeLtoR :: Char -> Enigma -> Char
encodeLtoR l (SimpleEnigma rL rM rR rf os) = reverseRotorEncode rR (os !! 0) (reverseRotorEncode rM (os !! 1) 
                                                                             (reverseRotorEncode rL (os !! 2) l))
 
                                                                             
{- incrementOffsets(Helper function). Takes a list of offsets.
   Returns a list of offsets after incrementing them appropriately.-}

incrementOffsets :: Offsets -> Offsets
incrementOffsets (x:xs) | (length (x:xs) == 0) = []
                        | (length (x:xs) == 1) = if (((x:xs) !! 0) == 25) then [0] else [((x:xs) !! 0 + 1)]
                        | (x == 25) = 0:(incrementOffsets xs)
                        | otherwise = (x + 1):xs
   
                        
{- steckerLetter(Helper function). Takes a steckerboard and a letter.
   Returns the letter that comes out of the steckerboard.
   sb = Steckerboard, l = Letter to be steckered-}    

steckerLetter :: Steckerboard -> Char -> Char
steckerLetter sb l | ((fst (findGuess l sb)) == l) = (snd (findGuess l sb))
                   | otherwise = (fst (findGuess l sb))  


{-**TEST DATA**-}
testMessage :: String
testMessage = "INXTHEXENIGMAXMACHINEXEACHXROTORXHADXAXNOTCHSTOPXINXTHEXSPECIFICATIONCOMMAXIXHAVEXASSUMEDXTHATXTHATXNOTCHXISXALWAYSXATXPOSITTIONXTWENTYFIVEXZXXWHEREASXINXREALITYXITXWASXATXAXDIFFERENTXPOSITIONXFORXEACHXROTORSTOPXWHENXAXKEYXWASXPRESSEDCOMMAXTHEXVERYXFIRSTXTHINGXTHATXHAPPENEDXWASXTHATXTHEXROTORSXWEREXADVANCEDSTOPXTHEXRIGHTXROTORXISXROTATEDXBYXONESTOPXIFXITXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMESXTHATXISXWASXATXPOSITIONXTWENTYFIVECOMMAXTHEXNOTCHXWOULDXCAUSEXTHEXMIDDLEXROTORXTOXALSOXROTATESTOPXIFXTHATXROTORXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMECOMMAXITXINXTURNXWOULDXCAUSEXTHEXLEFTXROTORXTOXROTATESTOPXINXOTHERXWORDSCOMMAXFORXTHEXMIDDLEXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXKEYXPRESSESSTOPXFORXTHEXLEFTXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSESSTOPXTOXGETXALLXTHEXWAYXBACKXTOXZEROCOMMAZEROCOMMAZEROCOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSEESSTOPTHEXDOWNSIDEXOFXTHEXSIMPLIFICATIONXTHATXIXHAVEXGIVENXISXTHATXTHEXONLINEXSIMULATORSXWILLXNOTXPROGRESSXTHEXSUBSEQUENTXROTORSXATXTHEXSAMEXTIMEXTHATXYOURXSIMULATIONXDOESSTOPXINXACTUALXFACTXROTORXONEXHADXAXNOTCHXATXPOSITIONXQCOMMAXROTORTWOXATXPOSITIONXECOMMAXROTORTHREEXATXPOSITIONXVCOMMAXROTORFOURXATXPOSITIONXJCOMMAXANDXROTORFIVEXATXPOSITIONXZSTOP"

{- enigmaEncode. Takes a letter and a simple enigma
   or a letter and a steckered enigma. Returns the encoded letter.
   lt = letter to be encoded, rL = left rotor, rM = middle rotor,
   rR = right rotor, os = offsets, sb = steckerboard-}  

enigmaEncode :: Char -> Enigma -> Char
enigmaEncode lt (SimpleEnigma rL rM rR rf os) = (encodeLtoR (letterSwap rf (encodeRtoL lt sE)) sE)
                                                        where
                                                        sE = (SimpleEnigma rL rM rR rf os)
enigmaEncode lt (SteckeredEnigma rL rM rR rf os sb) = steckerLetter sb (encodeLtoR (letterSwap rf (encodeRtoL (steckerLetter sb lt) sE)) sE)
                                                        where
                                                        sE = (SimpleEnigma rL rM rR rf os)
                                                        sTE = (SteckeredEnigma rL rM rR rf os sb)


{- enigmaEncodeMessage. Takes a message and a simple enigma
   or a message and a steckered enigma. Returns the encoded message.
   m = message to be encoded, rL = left rotor, rM = middle rotor,
   rR = right rotor, os = offsets, sb = steckerboard-}

enigmaEncodeMessage :: String -> Enigma -> String
enigmaEncodeMessage m (SimpleEnigma rL rM rR rf os) | (length m == 0) = []
                                                    | (length m == 1) = [enigmaEncode (m !! 0) simpleEnigma] 
                                                    | otherwise = (enigmaEncode (head m) simpleEnigma):(enigmaEncodeMessage (tail m) simpleEnigma)
                                                    where simpleEnigma = SimpleEnigma rL rM rR rf (incrementOffsets os)
enigmaEncodeMessage m (SteckeredEnigma rL rM rR rf os sb) | (length m == 0) = []
                                                          | (length m == 1) = [enigmaEncode (m !! 0) steckeredEnigma] 
                                                          | otherwise = (enigmaEncode (head m) steckeredEnigma):(enigmaEncodeMessage (tail m) steckeredEnigma)
                                                          where steckeredEnigma = SteckeredEnigma rL rM rR rf (incrementOffsets os) sb

{- Testing enigmaEncodeMessage/enigmaEncode :
Input: "INXTHEXENIGMAXMACHINEXEACHXROTORXHADXAXNOTCHSTOPXINXTHEXSPECIFICATIONCOMMAXIXHAVEXASSUMEDXTHATXTHATXNOTCHXISXALWAYSXATXPOSITTIONX
        TWENTYFIVEXZXXWHEREASXINXREALITYXITXWASXATXAXDIFFERENTXPOSITIONXFORXEACHXROTORSTOPXWHENXAXKEYXWASXPRESSEDCOMMAXTHEXVERYXFIRSTXTHI
        NGXTHATXHAPPENEDXWASXTHATXTHEXROTORSXWEREXADVANCEDSTOPXTHEXRIGHTXROTORXISXROTATEDXBYXONESTOPXIFXITXHADXALREADYXBEENXROTATEDXTWENT
        YFIVEXTIMESXTHATXISXWASXATXPOSITIONXTWENTYFIVECOMMAXTHEXNOTCHXWOULDXCAUSEXTHEXMIDDLEXROTORXTOXALSOXROTATESTOPXIFXTHATXROTORXHADXA
        LREADYXBEENXROTATEDXTWENTYFIVEXTIMECOMMAXITXINXTURNXWOULDXCAUSEXTHEXLEFTXROTORXTOXROTATESTOPXINXOTHERXWORDSCOMMAXFORXTHEXMIDDLEXR
        OTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXKEYXPRESSESSTOPXFORXTHEXLEFTXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYS
        IXXTIMESXTWENTYSIXXKEYXPRESSESSTOPXTOXGETXALLXTHEXWAYXBACKXTOXZEROCOMMAZEROCOMMAZEROCOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYS
        IXXTIMESXTWENTYSIXXKEYXPRESSEESSTOPTHEXDOWNSIDEXOFXTHEXSIMPLIFICATIONXTHATXIXHAVEXGIVENXISXTHATXTHEXONLINEXSIMULATORSXWILLXNOTXPR
        OGRESSXTHEXSUBSEQUENTXROTORSXATXTHEXSAMEXTIMEXTHATXYOURXSIMULATIONXDOESSTOPXINXACTUALXFACTXROTORXONEXHADXAXNOTCHXATXPOSITIONXQCOM
        MAXROTORTWOXATXPOSITIONXECOMMAXROTORTHREEXATXPOSITIONXVCOMMAXROTORFOURXATXPOSITIONXJCOMMAXANDXROTORFIVEXATXPOSITIONXZSTOP"
        enigmaEncodeMessage message^^ (SimpleEnigma rotor1 rotor2 rotor3 reflectorB [0,0,0])

Output:"HQRFMNYYVUUNBHACFQDZYSABBUEXJJGJPSFQGNTAJNLNZEIEPUSAXSYEKUBAHXLJZEUCGRFLYHUCDKDMKLZRPCQFMAHTGVYSSEYKUTWRXFHFMZRUWNNKCRTBNHSIOUWOD
        BTAZXPRSJALISVOTAFSFXETWMZRVFRLJNYCWYNMKVBGJTUJKDQBZTNBRSXUGDJRRBUWJBKVCAAWMSSFELVIPOHZTDGOXIZDQGHNLADFAXHVFKGQYASKCZEFAWFABPIITZ
        QPUWXJRHDFLLSMKIMVCIWEJCYSULAAWVQLOVHGJOKYFHIWVFBATADWVYARQBFEAWHLCKGDRXDRMSMNNBSKHFYIRSYHLQGCEQKIDQEXGIMHTUGHISMWQBWERWLGLEATJIJ
        PRWZJISCGDIVXJCRWJTCJNOFDEBXBGSRRICMQXUZHDVYQVFTNXQVCLOBCNZGKSQUFTAOZUHXURSKLKZFHBBYPQTDILBLXCOSAMFHNEGJPXXBCGAXVSRIVSWSRSQOWUAGZ
        SYVOAEMQHUOFJYKOGRFAXUQLYCPGCFMCOPIBIYGJJJZAFSJVSLRBAJZVWITZKJMFWSBGKTLVOCSWHTDSYVWYNHYZMNISJHSPLXTJGIQVNJHGYWLOCTXGCGKHAURIKBNSA
        MKLPJWQVAVZOHYNUBEPNAXILRQWDIQDYYPZVXBHLTLSSFXBJGJVVNHZHMWKLCWENMLOYDITLQCERPYNODYZLAPLYPLCEWOMJCEKSKRSAQKCLUMNBYWWWJAHHVEOYKXHOY
        YNUREFGGTVMJYMJLYUNQKMMWYJQMZXDFVFSIEKYVFTMMFAJSLBQBCKWBDUGKCJSJLRYHGADWCWMTSTKRGGPYRBOLPGZUVVKPRKCFAEJWWVVPWAHEGHKDAVPMXVHBLPWIV
        YILHKDSKWCSDWLVHRLOSUHCSKUDTAVIIFRXUFBWFYZLAQWBQJADGJOFDEFWGVXSKEYQCKCFTZWMBIQNWLRAXJOONXTNJQMZCREOIQZYYPIVIQEXFSHAZIOKYXJHJCHIWG
        WWZSIAYJPJVBKWDFKZUOUBYIGVMLCIZWIFKDELOULELFBUBUEJMUTMGTQUDIGIKZLZKNDGYQUAHODPSHEBEEOSNHUBTNPNAUQKJIZFYXHDQOQBXSCRWICRMGBETZKZBUR
        JCHITCUBFJJHSXOLXUQRGKWGJBPKNNODIBHFOCKYDEVRVZITAMPVZPZLEKFLZKHBVLYTWBFCCUWMXGLSRQALPJQPTISHPWDBQAMBMKSKIZCQCHLGPDUVRWYWW"   
        
Input:  enigmaEncodeMessage "" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB [0,0,0])

Output: ""-}





{- **Helper functions for longestMenu** -}
{- findIndexes(Helper function). Takes a character, starting index(0), 
   a string and a list of indexes. Returns a list of integers which are all
   the indexes where the letter is located in the string.
   l = letter, ci = current index, text = string potentially containing the letter,
   is = list of indexes-}

findIndexes :: Char -> Int -> String -> [Int] -> [Int]
findIndexes l ci text is | ((length text) == 0) = []
                         | ((length text) == 1) = if ((text !! 0) == l) then (tail (is ++ [ci]))
                                                    else tail is
                         | ((head text) == l) = findIndexes l (ci + 1) (tail text) (is ++ [ci])
                         | otherwise = findIndexes l (ci + 1) (tail text) is


{- findOpposite(Helper function). Takes a crib and an index.
   Returns the letter in the cipher text at that index of the crib.
   -} 

findOpposite :: Crib -> Int -> Char
findOpposite crib index = (snd crib) !! index


{-**TEST DATA**-}
crib1 :: Crib
crib1 = ("WETTERVORHERSAGEBISKAYA","RWIVTYRESXBFOGKUHQBAISE")


{- findSuccessors (Helper function). Takes an index and a crib.
   Returns a list of indexes for which the letter in the main text
   is equal to the letter in the cipher text at index n.
   n = given index, c = crib-}

findSuccessors :: [Int] -> Crib -> [[Int]]
findSuccessors n c | (length n == 0) = [[]]
                   | (length n == 1) = [(findIndexes (findOpposite c (head n)) 0 (fst c) [0])]
                   | otherwise = (findIndexes (findOpposite c (head n)) 0 (fst c) [0]):(findSuccessors (tail n) c)
            
                   
{- findMenus (Helper function). Takes an index, a crib and a menu.
   Returns a list of possible menus from the given index.
   n = index, c = crib, m = menu to be returned.-}

findMenus :: [Int] -> Crib -> Menu -> [Menu]
findMenus n c m | ((length n) == 0) = [m]
                | ((length n) == 1) && (elem (head n) m) = [m]
                | ((length n) == 1) && (not(elem (head n) m)) = (findMenus (head (findSuccessors [head n] c)) c ((head n):m))
                | (elem (head n) m) = (findMenus (tail n) c m)                                                           
                | otherwise = (findMenus (head (findSuccessors [head n] c)) c ((head n):m)) ++ (findMenus (tail n) c m)


{- findLongestMenu (Helper function). Takes a list of menus.
   Returns the longest menu is given list of menus.
   m = list of menus.-}

findLongestMenu :: [Menu] -> Menu
findLongestMenu m = head (sortBy (\xs ys -> compare (length ys) (length xs)) m)


{- findLongestMenuEachIndex (Helper function). Takes a starting index and a crib.
   Returns a list of menus which are the longest menus for each index from the
   starting index on.
   i = starting index, c = crib-}

findLongestMenuEachIndex :: Int -> Crib -> [Menu]
findLongestMenuEachIndex i c | (i == length(fst c)) = []
                             | (i == length(fst c) - 1) = [findLongestMenu (findMenus [i] c [])]
                             | otherwise = (findLongestMenu (findMenus [i] c [])):(findLongestMenuEachIndex (i + 1) c)


{- longestMenu. Takes a crib.
   Returns the longest possible menu in the crib.-}                             
longestMenu :: Crib -> Menu
longestMenu crib | length (fst crib) == 0 = []
                 | otherwise = reverse(findLongestMenu(findLongestMenuEachIndex 0 crib))

{- Testing longestMenu :
Input:  longestMenu ("WETTERVORHERSAGEBISKAYA","RWIVTYRESXBFOGKUHQBAISE") 
Output: [13,14,19,22,1,0,5,21,12,7,4,3,6,8,18,16,9]  

Input:  longestMenu ("","")
Output: []

Input:  longestMenu ("F","U")
Output: [0]  -}