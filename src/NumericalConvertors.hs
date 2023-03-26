{-
Project: FLP 2023 Haskell Project
File: NumericalConvertors.hs
Author: Vojtech Fiala <xfiala61>
-}

module NumericalConvertors where

-- function to convert integer to its binary representation in a string and reverse it
decToBin :: Integer -> String
decToBin x = reverse (binaryHelp x)
    where 
        binaryHelp :: Integer -> String
        binaryHelp 0 = "0" 
        binaryHelp 1 = "1"
        binaryHelp a = binaryHelp (a `div` 2) ++ show (a `mod` 2)

-- function to convert an integer to its byte representation, aka 1024 = 0x0400
decToHex :: Integer -> String
decToHex x = hexHelp x
    where 
        hexHelp :: Integer -> String
        hexHelp 0 = "0"
        hexHelp 1 = "1"
        hexHelp 2 = "2"
        hexHelp 3 = "3"
        hexHelp 4 = "4"
        hexHelp 5 = "5"
        hexHelp 6 = "6"
        hexHelp 7 = "7"
        hexHelp 8 = "8"
        hexHelp 9 = "9"
        hexHelp 10 = "a"
        hexHelp 11 = "b"
        hexHelp 12 = "c"
        hexHelp 13 = "d"
        hexHelp 14 = "e"
        hexHelp 15 = "f"
        hexHelp n = hexHelp (n `div` 16) ++ hexHelp (n `mod` 16)
