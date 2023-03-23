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


-- function to get the `b` from Right b
rightPart :: Either a b -> b
rightPart (Right b) = b
rightPart (Left _) = error "There was an error in parsing the Curve!"

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

-- function to convert integer to its binary representation in a string
binary :: Integer -> String
binary 0 = "0"
binary 1 = "1"
binary x = binary (x `div` 2) ++ show (x `mod` 2)

-- https://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication#Double-and-add
-- function to calculate point multiplicatiojn using double and add algo
doubleAndAdd :: Types.Point -> Types.Point -> Integer -> String -> Types.Point
doubleAndAdd (Types.Point tempx tempy) (Types.Point resx resy) a privateStr
    | privateStr == "" = Types.Point resx resy
    | otherwise = if head privateStr == '1' then doubleAndAdd newTemp newRes a newStr else doubleAndAdd newTemp (Types.Point resx resy) a newStr
    where newTemp = pointAdd (Types.Point tempx tempy) (Types.Point tempx tempy) a
          newRes = pointAdd (Types.Point resx resy) (Types.Point tempx tempy) a
          newStr = tail privateStr

-- function to calculate the public key
-- TADY VYRESIT TEN FUCKED UP ZPICENY FORMAT nejak s x a y
getPublic :: Types.Point -> Integer
getPublic (Types.Point x y) = x

mDiff :: Integer -> Integer -> Integer -> Integer -> Integer
mDiff xp yp xq yq = ((yq - yp) `div` (xq - xp))

mSame :: Integer -> Integer -> Integer -> Integer
mSame xp a yp = ((3*xp^2 + a) `div` (2*yp))

-- https://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication
-- function to add 2 points
pointAdd :: Types.Point -> Types.Point -> Integer -> Types.Point
pointAdd (Types.Point xp yp) (Types.Point xq yq) a
    | (xp == xq && yq == yp) = Types.Point xr1 (m1*(xp-xr1)-yp)
    | otherwise = Types.Point xr2 (m2*(xp-xr2)-yp)
    where m1 = mSame xp a yp
          xr1 = m1^2 - xp - xq
          m2 = mDiff xp yp xq yq
          xr2 = m2^2 - xp - xq

-- function to calculate private & public key pair from given eliptic Curve
calculateKey :: Types.Curve -> Types.Key
calculateKey (Types.Curve p a b g n h) =
    Types.Key randomPrivate public
    where randomPrivate = getRandomInInterval (n-1)
          public = getPublic(doubleAndAdd g (Types.Point 0 0) a (binary randomPrivate))

-- function to do get the appropriate output depending on the given argument
actOnParameter :: [String] -> String -> IO ()
actOnParameter args input
    | arg == "-i" = print (rightPart(P.parse PS.parseCurve "" input)) -- call the instance of Show of the Curve class and output it
    | arg == "-k" = print (calculateKey (rightPart(P.parse PS.parseCurve "" input)))
    | arg == "-s" =  putStrLn "-s"
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
