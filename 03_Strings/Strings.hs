--3.8 Chapter Exercises

--Building Functions
  
--3-

    thirdLetter :: String -> Char
    thirdLetter str = head $ drop 2 str

--4- 

    letterIndex :: String -> Int -> Char
    letterIndex x n = head $ drop n x

--5-

    rvrs a = 
        let b = take 5 a
            aa = drop 6 a
            c = take 2 aa
            aaa = drop 3 aa
        in aaa ++ " " ++ c ++ " " ++ b 

