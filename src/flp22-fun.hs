{-
Project: FLP 2023 Haskell Project
File: flp22-fun.hs
Author: Vojtech Fiala <xfiala61>
-}

import qualified Types
import qualified Math as M
import qualified NumericalConvertors as N
import qualified ParseStructures as PS
import qualified System.IO as I
import qualified System.Environment as E
import qualified System.Exit as Exit
import qualified Text.Parsec as P
import qualified System.Random as Random

-- import Debug.Trace


-- function to get the `b` from Right b, only used in getting the Parsec output
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

-- function to calculate private & public key pair from given eliptic Curve
calculateKey :: Types.Curve -> Integer -> Types.Key
calculateKey (Types.Curve p a _ g _ _) random =
    Types.Key random public
    where
        public = getPublic(M.doubleAndAdd g (Types.Point 0 0) a p (N.decToBin random))

-- function to create the public key from the given X and Y coords
getPublic :: Types.Point -> String
getPublic (Types.Point x y) 
    | (y > 0) = if length(decX ++ decY) `mod` 2 == 1 then "0x04" ++ "0" ++ decX ++ decY else "0x04" ++ decX ++ decY
    | otherwise = if length(decX ++ N.decToHex(-y)) `mod` 2 == 1 then "0x04" ++ "0" ++ decX ++ N.decToHex(-y) else "0x04" ++ decX ++ N.decToHex(-y) -- if Y is negative, make it positive
    where 
        decY = N.decToHex (y)
        decX = N.decToHex (x)

-- function to calculate the X parameter in the signature
-- https://cs.wikipedia.org/wiki/Protokol_digit%C3%A1ln%C3%ADho_podpisu_s_vyu%C5%BEit%C3%ADm_eliptick%C3%BDch_k%C5%99ivek
getSignatureR :: Types.Curve -> Integer -> Integer
getSignatureR (Types.Curve p a _ g n _) k = getX(M.doubleAndAdd g (Types.Point 0 0) a p (N.decToBin k))
    where 
        getX (Types.Point x _) = x `mod` n

-- function to calculate the X parameter in the signature
-- https://cs.wikipedia.org/wiki/Protokol_digit%C3%A1ln%C3%ADho_podpisu_s_vyu%C5%BEit%C3%ADm_eliptick%C3%BDch_k%C5%99ivek
getSignatureS :: Types.Curve -> Types.Key -> Types.Hash -> Integer -> Integer -> Integer
getSignatureS (Types.Curve _ _ _ _ n _) (Types.Key priv _) (Types.Hash hash) r k = ((M.modularInverse k n) * (hash + r*priv)) `mod` n
      
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
splitKey a = Types.Point x y -- i count on the pubkey being even n of chars long, otherwise problems might occur
    where
        half = (length a) `div` 2
        x = read("0x" ++ take half a) -- take first half values and prepend 0x so that it reads correct
        y = read("0x" ++ drop half a) -- drop the first half so that only the second half remain

isValidPub :: Types.Point -> Types.Curve -> Bool
isValidPub point (Types.Curve p a _ _ n _) = (M.doubleAndAdd point (Types.Point 0 0) a p (N.decToBin n) == (Types.Point 0 0))

-- function to verify the signature
-- as always, I assume the format is correct
-- https://cs.wikipedia.org/wiki/Protokol_digit%C3%A1ln%C3%ADho_podpisu_s_vyu%C5%BEit%C3%ADm_eliptick%C3%BDch_k%C5%99ivek
verifySignature :: Types.Vstruct -> Bool
verifySignature (Types.Vstruct (Types.Curve p a b g n h) (Types.Signature r s) pubkey (Types.Hash hash)) 
    | validity == False = False
    | otherwise = ((getX point) == r)
    where 
        pubPoint = getPoint pubkey
        validity = isValidPub pubPoint (Types.Curve p a b g n h)
        getX (Types.Point x _) = x
        w = M.modularInverse s n 
        u1 = w*hash `mod` n
        u2 = w*r `mod` n
        point = (M.pointAdd(M.doubleAndAdd g (Types.Point 0 0) a p (N.decToBin u1)) (M.doubleAndAdd pubPoint (Types.Point 0 0) a p (N.decToBin u2)) a p)

-- function to get the appropriate output depending on the given argument
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
