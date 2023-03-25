{-
Project: FLP 2023 Haskell Project
File: flp22-fun.hs
Author: Vojtech Fiala <xfiala61>
-}

import qualified Types
import qualified Data.Char as D
import qualified ParseStructures as PS
import qualified System.IO as I
import qualified System.Environment as E
import qualified System.Exit as Exit
import qualified Text.Parsec as P
import qualified System.Random as Random

--import Debug.Trace


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
-- because of Haskell and its weird IO stuff, this will keep producing the same number because I cant do anything else
getRandomInInterval :: Integer -> Integer
getRandomInInterval top =
    fst(Random.uniformR(1 :: Integer, (top-1) :: Integer) gen)
    where gen = Random.mkStdGen 42

-- function to convert integer to its binary representation in a string and reverse it
binary :: Integer -> String
binary x = reverse (binaryHelp x)
    where binaryHelp 0 = "0" 
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
    where newTemp = pointAdd tmpPoint tmpPoint a p
          newRes = pointAdd resultPoint tmpPoint a p
          newStr = tail privateStr

{- -- version with foldr, looks better but doesnt work (perhaps fix later?)
doubleAndAdd :: Types.Point -> Types.Point -> Integer -> Integer -> String -> Types.Point
doubleAndAdd tmpPoint resultPoint a p privateStr =
  snd(foldr f (tmpPoint, resultPoint) privateStr)
  where
        f :: Char -> (Types.Point, Types.Point) -> (Types.Point, Types.Point)
        f '1' (tmpPoint, resultPoint) = ((pointAdd tmpPoint tmpPoint a p), (pointAdd resultPoint tmpPoint a p))
        f '0' (tmpPoint, resultPoint) = ((pointAdd tmpPoint tmpPoint a p), resultPoint)
-}

-- function to simulate python-like pow(x,y,z)
-- https://www.reddit.com/r/haskell/comments/mqtk6/comment/c33n70a/?utm_source=share&utm_medium=web2x&context=3
-- incredibly more effective than x^y % p
fastPow :: Integer -> Integer -> Integer -> Integer
fastPow base exp' modulo = fastpow' (base `mod` modulo) exp' modulo 1
    where fastpow' _ 0 _ r = r
          fastpow' b e m r = fastpow' (b * b `mod` m) (e `div` 2) m (if even e then r else (r * b `mod` m))

-- https://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication
-- function to add 2 points
pointAdd :: Types.Point -> Types.Point -> Integer -> Integer -> Types.Point
pointAdd (Types.Point xp yp) (Types.Point xq yq) a p
    | xp == 0 && yp == 0 = Types.Point xq yq
    | xq == 0 && yq == 0 = Types.Point xp yp
    | (xp == xq && yq == yp) = if yp == 0 then Types.Point 0 0 else Types.Point xr1 ((m1*(xp-xr1)-yp) `mod` p)
    | xp == xq = Types.Point 0 0
    | otherwise = Types.Point xr2 ((m2*(xp-xr2)-yp) `mod` p)
    where m1 = mSame xp a yp p
          xr1 = (m1^(2::Integer) - xp - xq) `mod` p
          m2 = mDiff xp yp xq yq p
          xr2 = (m2^(2::Integer) - xp - xq) `mod` p

mDiff :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
mDiff xp yp xq yq p = (((yq - yp) * (fastPow (xq - xp) (p-2) p)) `mod` p)

mSame :: Integer -> Integer -> Integer -> Integer -> Integer
mSame xp a yp p = (((3*xp*xp + a) * (fastPow (2*yp) (p-2) p)) `mod` p)

-- function to convert an integer to its byte representation, aka 1024 = 0x0400
-- https://www.tutorialspoint.com/haskell-program-to-convert-decimal-to-hexadecimal
decToHex :: Integer -> String
decToHex 0 = "0"
decToHex n = reverse (hexChars n)
   where
      hexChars :: Integer -> String
      hexChars 0 = ""
      hexChars x = D.intToDigit (fromInteger (x `mod` 16)) : hexChars (x `div` 16)

-- function to calculate private & public key pair from given eliptic Curve
calculateKey :: Types.Curve -> Types.Key
calculateKey (Types.Curve p a _ g n _) =
    Types.Key randomPrivate public
    where randomPrivate = getRandomInInterval (n-1)
          public = getPublic(doubleAndAdd g (Types.Point 0 0) a p (binary randomPrivate))

-- function to create the public key from the given X and Y coords
getPublic :: Types.Point -> String
getPublic (Types.Point x y) 
    | (y > 0) = if length(decX ++ decY) `mod` 2 == 1 then "0x04" ++ "0" ++ decX ++ decY else "0x04" ++ decX ++ decY
    | otherwise = if length(decX ++ decToHex(-y)) `mod` 2 == 1 then "0x04" ++ "0" ++ decX ++ decToHex(-y) else "0x04" ++ decX ++ decToHex(-y)
    where decY = decToHex (y)
          decX = decToHex (x)

-- function to calculate the X parameter in the signature
getSignatureX :: Types.Curve -> Integer -> Integer
getSignatureX (Types.Curve p a _ g n _) k = getX(doubleAndAdd g (Types.Point 0 0) a p (binary k))
        where getX (Types.Point x y) = x `mod` n

-- function to calculate the X parameter in the signature
getSignatureY :: Types.Curve -> Types.Key -> Types.Hash -> Integer -> Integer -> Integer
getSignatureY (Types.Curve p a b g n h) (Types.Key priv pub) (Types.Hash hash) r k = 0

-- function to calculate modularInverse
-- python code which was linked on the forum doesnot understand that k^-1 doesnt m,ean 1/k ion this case but multiplicate inverse of k
-- shamelessly taken from https://byorgey.wordpress.com/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1/
modularInverse :: Integer -> Integer -> Integer
modularInverse x y = z `mod` x
        where (_,_,z) = egcd y x

-- shamelessly taken from https://byorgey.wordpress.com/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1/
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0 = (abs a, signum a, 0)
egcd a b = (g, y, x - (a `div` b) * y)
    where
        (g,x,y) = egcd b (a `mod` b)

-- function to calculate signature
calculateSignature :: Types.Sstruct -> Types.Signature
calculateSignature (Types.Sstruct (Types.Curve p a b g n h) key hash) =
    Types.Signature r s
    where k = getRandomInInterval (n-1)
          r = getSignatureX (Types.Curve p a b g n h) k
          s = getSignatureY (Types.Curve p a b g n h) key hash r (modularInverse k n)


-- function to do get the appropriate output depending on the given argument
actOnParameter :: [String] -> String -> IO ()
actOnParameter args input
    | arg == "-i" = print (rightPart (P.parse PS.parseCurve "" input)) -- call the instance of Show of the Curve class and output it
    | arg == "-k" = print (calculateKey (rightPart(P.parse PS.parseCurve "" input)))
    | arg == "-s" = print (calculateSignature (rightPart(P.parse PS.parseSstruct "" input)))
    | arg == "-v" = putStrLn "-v"
    | arg == "-h" = printHelp
    | otherwise = do
        printHelp
        putStrLn "\nUnknown parameter!"
        Exit.exitWith (Exit.ExitFailure 1)
    where arg = head args

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
