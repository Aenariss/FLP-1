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

testParse :: String -> Either P.ParseError Types.Curve
testParse input = P.parse PS.parseCurve "" input

-- function to get the `b` from Right b
rightPart :: Either a b -> b
rightPart (Right b) = b
rightPart (Left _) = error "There was an error in parsing the input!"

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

-- function to do get the appropriate output depending on the given argument
actOnParameter :: [String] -> String -> IO ()
actOnParameter args input
    | arg == "-i" = print (rightPart(P.parse PS.parseCurve "" input)) -- call the instance of Show of the Curve class and output it
    | arg == "-k" =  putStrLn "-k"
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
