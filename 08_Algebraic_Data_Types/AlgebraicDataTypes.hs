

import Data.Char 
    ( ord, chr, isUpper, toUpper, isSpace, isLetter, isDigit, isLower, isUpper, toUpper )
import Text.ParserCombinators.ReadPrec (prec)
import Data.List (elemIndices)


--11.18 Chapter Exercises

---

--Multiple choice


--Ciphers


caesar :: [Char] -> Int -> [Char]
unCaesar :: [Char] -> Int -> [Char]
rightward :: Char -> Int -> Char
leftward :: Char -> Int -> Char

rightward ch n 
    | ord ch >= 65 && ord ch <= 90 =  
        if (ord ch + (snd $ divMod n 26)) <= 90 
            then chr $ ord ch + (snd $ divMod n 26)
        else chr $ (ord ch + (snd $ divMod n 26)  - succ 90) + 65
    |  ord ch >= 97 && ord ch <= 122 = 
        if (ord ch + (snd $ divMod n 26)) <= 122 
            then chr $ ord ch + (snd $ divMod n 26)
        else chr $ (ord ch + (snd $ divMod n 26)  - succ 122) + 97
    |  otherwise = ch



leftward ch n 
    | ord ch >= 65 && ord ch <= 90 =  
        if (ord ch - (snd $ divMod n 26)) >= 65 
            then chr $ ord ch - (snd $ divMod n 26)
        else chr $ 90 - (pred 65 - (ord ch - (snd $ divMod n 26)))
    |  ord ch >= 97 && ord ch <= 122 = 
         if (ord ch - (snd $ divMod n 26)) >= 97 
            then chr $ ord ch - (snd $ divMod n 26)
        else chr $ 122 - (pred 97 - (ord ch - (snd $ divMod n 26)))
    |  otherwise = ch

caesar  xs n = map  (`rightward`n) xs
unCaesar xs n   = map  (`leftward`n) xs  


coupleithIndex ::  String -> [(Char,Int)]
coupleithIndex  = foldl foldFunction [] 
    where foldFunction acc u 
            | u == ' ' = if length acc == 0 then acc ++ [(u, -1)] else acc ++ [(u, snd (last acc))]
            | otherwise = if (length acc == 0) then acc ++ [(u,  0)] 
                                                  else acc ++ [(u,  1 + snd (last acc))]

extendedKey :: String -> String -> String
extendedKey text key = foldr foldFunction [] (coupleithIndex text)
    where foldFunction couple  acc 
            | fst couple == ' ' = fst couple:acc
            | otherwise = (key !! snd (divMod (snd couple) (length key))):acc
 


--Phone exercise


data PhoneLayout = PhoneLayout [[String]]
phoneLayout :: PhoneLayout

type DaPhone = PhoneLayout --[[String]]

phoneLayout = PhoneLayout [
        [
            "1", "2 ABC", "3 DEF"
        ],
        [
            "4 GHI", "5 JKL", "6 MNO"
        ],
        [
            "7 PQRS", "8 TUV", "9 WXYZ"
        ],
        [
            "* ^", "0 + _", "# .,"
        ]
    ]

convo :: [String]
convo =[
        "Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Just making sure rofl ur turn"
    ]

type Digit = Char

type Presses = Int
{--
getDigitPresses :: [String] -> Char -> [(Digit, Presses)]
getDigitPresses list caractere = foldr f [] list 
                             where f bouton acc 
                                    | isDigit caractere =  if caractere `elem` bouton then  acc ++ [(head bouton, 1)] else acc  
                                    | isLower caractere =  if toUpper caractere `elem` bouton then acc ++ [(head bouton,  (head $ elemIndices (toUpper caractere) bouton) -1)] else acc
                                    | otherwise = if toUpper caractere `elem` bouton then acc ++ [('*', 1), (head bouton, (head $ elemIndices (toUpper caractere) bouton) -1)] else acc
--}

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (PhoneLayout myphoneLayout) caractere = foldr foldFunction [] myphoneLayout
                                    where foldFunction boutons acc =
                                            acc ++ foldr f [] boutons
                                              where f bouton lacc
                                                        | isSpace caractere = if caractere `elem` bouton then lacc ++ [('0', 1)] else lacc
                                                        | isDigit caractere =  if caractere `elem` bouton then  lacc ++ [(head bouton, 1)] else lacc
                                                        | isLetter caractere && isLower caractere =  if toUpper caractere `elem` bouton then lacc ++ [(head bouton,  (head $ elemIndices (toUpper caractere) bouton) -1)] else lacc
                                                        | isLetter caractere && isUpper caractere = if toUpper caractere `elem` bouton then lacc ++ [('*', 1), (head bouton, (head $ elemIndices (toUpper caractere) bouton) -1)] else lacc
                                                        | otherwise = if caractere `elem` bouton then  lacc ++ [(head bouton, 1)] else lacc


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]

cellPhonesDead daPhone  = concatMap $ reverseTaps  daPhone



--Hutton’s Razor

data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add exp1 exp2) = eval exp1 + eval exp2



