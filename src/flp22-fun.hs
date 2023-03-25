{-
Project: FLP 2023 Haskell Project
File: flp22-fun.hs
Author: Vojtech Fiala <xfiala61>
-}

import qualified Types
import qualified ParseStructures as PS
import qualified System.IO as I
import qualified System.Environment as E
import qualified System.Exit as Exit
import qualified Text.Parsec as P
import qualified System.Random as Random

import Debug.Trace


-- function to get the `b` from Right b
rightPart :: Either a b -> b
rightPart (Right b) = b
rightPart (Left _) = error "There was an error in parsing the Input!"

-- function to print Help
printHelp :: IO ()
printHelp = do
    putStrLn "flp22-fun - ECDSA\n*****************"
    putStrLn "  - This program requires 1 user parameter and reads input from stdin."
    putStrLn "  - Additional second argument may be used to refer to file from which the content will be read instead of stdin"
    putStrLn "  - Valid parameters (of which only 1 must be used at the time) are [-i|-k|-s|-v|-h] where:"
    putStrLn "      -i loads the input into the data structure and prints it out"
    putStrLn "      -k loads the input and prints out the generated key pair"
    putStrLn "      -s loads the input with a private key and a hash and prints out the generated signature"
    putStrLn "      -v loads the input with a public key and a hash and checks if it matches the signature"
    putStrLn "      -h prints this menu"

-- function to generate 'random' nubmer in range 1-Top
getRandomInInterval :: Types.Curve -> IO Integer
getRandomInInterval (Types.Curve _ _ _ _ n _) = Random.randomRIO(1, (n-1))

-- same as above but for Sstruct
getRandomInIntervalS :: Types.Sstruct -> IO Integer
getRandomInIntervalS (Types.Sstruct (Types.Curve _ _ _ _ n _) _ _) = Random.randomRIO(1, (n-1))

-- function to convert integer to its binary representation in a string and reverse it
binary :: Integer -> String
binary x = reverse (binaryHelp x)
    where 
        binaryHelp :: Integer -> String
        binaryHelp 0 = "0" 
        binaryHelp 1 = "1"
        binaryHelp a = binaryHelp (a `div` 2) ++ show (a `mod` 2)

-- https://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication#Double-and-add
-- function to calculate point multiplication using double and add algo
doubleAndAdd :: Types.Point -> Types.Point -> Integer -> Integer -> String -> Types.Point
doubleAndAdd tmpPoint resultPoint a p privateStr
    | privateStr == "" = resultPoint
    | otherwise = if head privateStr == '1' 
                    then doubleAndAdd newTemp newRes a p newStr 
                    else doubleAndAdd newTemp resultPoint a p newStr
    where 
        newTemp = pointAdd tmpPoint tmpPoint a p
        newRes = pointAdd resultPoint tmpPoint a p
        newStr = tail privateStr

-- function to simulate python-like pow(x,y,z)
-- incredibly more effective than x^y % p
-- https://www.geeksforgeeks.org/exponential-squaring-fast-modulo-multiplication/ -based on python ver
pow :: Integer -> Integer -> Integer -> Integer
pow base exp' modul 
    | exp' == 0 = 1
    | exp' == 1 = base `mod` modul
    | (exp' `mod` 2 == 0) = t
    | otherwise = ((base `mod` modul) * t) `mod` modul
    where 
        t = (pow base (exp' `div` 2) modul)^(2::Integer) `mod` modul

-- https://stackoverflow.com/questions/31074172/elliptic-curve-point-addition-over-a-finite-field-in-python
-- function to add 2 points
pointAdd :: Types.Point -> Types.Point -> Integer -> Integer -> Types.Point
pointAdd (Types.Point xp yp) (Types.Point xq yq) a p
    | xp == 0 && yp == 0 = Types.Point xq yq
    | xq == 0 && yq == 0 = Types.Point xp yp
    | (xp == xq && yq == yp) = if yp == 0 then Types.Point 0 0 else Types.Point xr1 ((m1*(xp-xr1)-yp) `mod` p)
    | xp == xq = Types.Point 0 0
    | otherwise = Types.Point xr2 ((m2*(xp-xr2)-yp) `mod` p)
    where 
        m1 = ((3*xp*xp + a) * (pow (2*yp) (p-2) p)) `mod` p
        xr1 = (m1^(2::Integer) - xp - xq) `mod` p
        m2 = ((yq - yp) * (pow (xq - xp) (p-2) p)) `mod` p
        xr2 = (m2^(2::Integer) - xp - xq) `mod` p

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

-- function to calculate private & public key pair from given eliptic Curve
calculateKey :: Types.Curve -> Integer -> Types.Key
calculateKey (Types.Curve p a _ g _ _) random =
    Types.Key random public
    where
        public = getPublic(doubleAndAdd g (Types.Point 0 0) a p (binary random))

-- function to create the public key from the given X and Y coords
getPublic :: Types.Point -> String
getPublic (Types.Point x y) 
    | (y > 0) = if length(decX ++ decY) `mod` 2 == 1 then "0x04" ++ "0" ++ decX ++ decY else "0x04" ++ decX ++ decY
    | otherwise = if length(decX ++ decToHex(-y)) `mod` 2 == 1 then "0x04" ++ "0" ++ decX ++ decToHex(-y) else "0x04" ++ decX ++ decToHex(-y) -- if Y is negative, make it positive
    where 
        decY = decToHex (y)
        decX = decToHex (x)

-- function to calculate the X parameter in the signature
-- https://cs.wikipedia.org/wiki/Protokol_digit%C3%A1ln%C3%ADho_podpisu_s_vyu%C5%BEit%C3%ADm_eliptick%C3%BDch_k%C5%99ivek
getSignatureR :: Types.Curve -> Integer -> Integer
getSignatureR (Types.Curve p a _ g n _) k = getX(doubleAndAdd g (Types.Point 0 0) a p (binary k))
    where 
        getX (Types.Point x _) = x `mod` n

-- function to calculate the X parameter in the signature
-- https://cs.wikipedia.org/wiki/Protokol_digit%C3%A1ln%C3%ADho_podpisu_s_vyu%C5%BEit%C3%ADm_eliptick%C3%BDch_k%C5%99ivek
getSignatureS :: Types.Curve -> Types.Key -> Types.Hash -> Integer -> Integer -> Integer
getSignatureS (Types.Curve _ _ _ _ n _) (Types.Key priv _) (Types.Hash hash) r k = ((modularInverse k n) * (hash + r*priv)) `mod` n

-- function to calculate modular multiplicartive inverse
-- https://stackoverflow.com/questions/4798654/modular-multiplicative-inverse-function-in-python
modularInverse :: Integer -> Integer -> Integer
modularInverse a m = x `mod` m
    where
        (_, x, _) = egcd a m

-- function to calculate modular multiplicartive inverse
-- https://stackoverflow.com/questions/4798654/modular-multiplicative-inverse-function-in-python
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a b 
    | a == 0 = (b, 0, 1)
    | otherwise = (g, (x - (b `div` a) * y), y)
    where (g, y, x) = egcd (b `mod` a) a
        
-- function to calculate signature
calculateSignature :: Types.Sstruct -> Integer -> Types.Signature
calculateSignature (Types.Sstruct (Types.Curve p a b g n h) key hash) k =
    Types.Signature r s
    where 
        r = getSignatureR (Types.Curve p a b g n h) k
        s = getSignatureS (Types.Curve p a b g n h) key hash r k

-- function to create a point with x and y coords from pubkey
getPoint :: String -> Types.Point 
getPoint a = splitKey pureKey
    where 
        pureKey = drop 4 a -- remove 0x04

-- function to split the pubkey into 2 coords
splitKey :: String -> Types.Point
splitKey a = Types.Point x y -- i count on the pubkey being 128 chars long
    where
        x = read("0x" ++ take 64 a) -- take first 64 values and prepend 0x so that it reads correct
        y = read("0x" ++ drop 64 a)


isValidPub :: Types.Point -> Types.Curve -> Bool
isValidPub point (Types.Curve p a _ _ n _) = (doubleAndAdd point (Types.Point 0 0) a p (binary n) == (Types.Point 0 0))

-- function to verify the signature
-- as always, I assume the format is correct
verifySignature :: Types.Vstruct -> Bool
verifySignature (Types.Vstruct (Types.Curve p a b g n h) (Types.Signature r s) pubkey (Types.Hash hash)) 
    | validity == False = False
    | otherwise = ((getX point) == r)
    where 
        pubPoint = getPoint pubkey
        validity = isValidPub pubPoint (Types.Curve p a b g n h)
        getX (Types.Point x _) = x
        w = modularInverse s n 
        u1 = w*hash `mod` n
        u2 = w*r `mod` n
        point = (pointAdd(doubleAndAdd g (Types.Point 0 0) a p (binary u1)) (doubleAndAdd pubPoint (Types.Point 0 0) a p (binary u2)) a p)

-- function to do get the appropriate output depending on the given argument
actOnParameter :: [String] -> String -> IO ()
actOnParameter args input
    | arg == "-i" = print (rightPart (P.parse PS.parseCurve "" input)) -- call the instance of Show of the Curve class and output it
    | arg == "-k" = do
        let res = rightPart(P.parse PS.parseCurve "" input)
        random <- getRandomInInterval res -- get a random number, must be from inside IO block
        print (calculateKey res random)
    | arg == "-s" = do
        let res = rightPart(P.parse PS.parseSstruct "" input)
        random <- getRandomInIntervalS res
        print(calculateSignature res random)
    | arg == "-v" = print (verifySignature(rightPart(P.parse PS.parseVstruct "" input)))
    | arg == "-h" = printHelp
    | otherwise = do
        printHelp
        putStrLn "\nUnknown parameter!"
        Exit.exitWith (Exit.ExitFailure 1)
    where 
        arg = head args

-- function to parse the arguments that calls the correct function based on the parameter
parseArgs :: [String] -> IO () 
parseArgs args 
    -- only 1 arg - act upon which one it was
    | (length args) == 1 = do
        input <- getContents -- read stdin
        actOnParameter args input
        Exit.exitWith Exit.ExitSuccess
    -- 2 arguments should be the parameter and the file from which to read
    | (length args) == 2 = do
        fileHandle <- I.openFile (args !! 1) I.ReadMode
        input <- I.hGetContents fileHandle
        actOnParameter args input
        I.hClose fileHandle
        Exit.exitWith Exit.ExitSuccess
    -- otherwise I expect the input to be incorrect and in order to help the user, I show him the help menu
    | otherwise = do
        printHelp
        putStrLn "\nToo many or too few arguments!"
        Exit.exitWith (Exit.ExitFailure 1)

-- main function to call the other functions
main :: IO ()
main = do
        args <- E.getArgs
        parseArgs args
