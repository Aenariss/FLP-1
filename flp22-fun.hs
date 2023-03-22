import qualified Types
import qualified System.Environment as E
import qualified System.Exit as Exit
import qualified Text.Parsec as P
import qualified Text.Parsec.String as Str

testParse :: String -> Either P.ParseError Types.Point
testParse input = P.parse parsePoint "" input

parseArgs :: [String] -> IO () 
parseArgs args 
    | (length args) < 2 = do
        putStrLn "Too few arguments!" 
        Exit.exitWith (Exit.ExitFailure 1)
    | (length args) == 2 = do
        putStrLn "Opening file!" 
        Exit.exitWith Exit.ExitSuccess
    | otherwise = do
        let res1 = testParse ( unwords (args) )
        print(res1)
        Exit.exitWith Exit.ExitSuccess

-- main function to do IO stuff
main :: IO ()
main =  do
        args <- E.getArgs
        print(args)
        parseArgs args

-- function to parse the Point structure
-- ^\s*Point\s*{\s*x\s*:\s*([0-9]|[A-F])+\s*,\s*y\s*:\s*([0-9]|[A-F])+\s*}
parsePoint :: Str.Parser Types.Point
parsePoint = do
    _ <- P.string "Point" -- has to start w/ "Point"
    _ <- P.spaces
    _ <- P.char '{'
    _ <- P.spaces
    x <- hexParse "x" -- x value
    _ <- P.spaces
    _ <- P.char ','
    _ <- P.spaces
    y <- hexParse "y" -- y value
    _ <- P.spaces
    _ <- P.char '}'
    return (Types.Point x y) -- call a constructor using the 2

-- function to read a value that contains a hex number
-- ^val\s*:\s*([0-9]|[A-F])+
hexParse :: String -> Str.Parser String
hexParse valName = do
    _ <- P.string valName --first, find the correct value
    _ <- P.spaces -- dont care about number of spaces
    _ <- P.char ':' -- there has to be a ':' somewhere
    _ <- P.spaces
    givenVal <- P.many1 (P.hexDigit) -- read while the char is a valid hex value
    return givenVal
