{-
Project: FLP 2023 Haskell Project
File: ParseStructures.hs
Author: Vojtech Fiala <xfiala61>
-}

module ParseStructures where

import qualified Text.Parsec as P
import qualified Text.Parsec.String as Str
import qualified Types

-- function to parse the Curve
-- ^\s*Curve\s*{\s* parseHex(p) \s* parseInt(a) \s* parseInt(b) \s* parsePoint() \s* parseHex(n) \s* parseInt(h)}
parseCurve :: Str.Parser Types.Curve
parseCurve = do
    _ <- P.string "Curve"
    _ <- P.spaces
    _ <- P.char '{'
    _ <- P.spaces
    p <- parseHex "p"
    _ <- P.spaces
    a <- parseInt "a"
    _ <- P.spaces
    b <- parseInt "b"
    _ <- P.spaces
    g <- parsePoint "g"
    _ <- P.spaces
    n <- parseHex "n"
    _ <- P.spaces
    h <- parseInt "h"
    _ <- P.spaces
    _ <- P.char '}'
    return (Types.Curve (read(p)) (read(a)) (read(b)) g (read(n)) (read(h))) -- im counting on the integers always being valid, so I just read them which gives me the value

-- function to parse the Point structure
-- ^\s*Point\s*{\s*x\s*:\s*([0-9]|[A-F])+\s*\s*y\s*:\s*([0-9]|[A-F])+\s*}
parsePoint :: String -> Str.Parser Types.Point
parsePoint valName = do
    _ <- P.string valName --first, find the correct value
    _ <- P.spaces -- dont care about number of spaces
    _ <- P.char ':' -- there has to be a ':' somewhere
    _ <- P.spaces -- dont care about number of spaces
    _ <- P.string "Point" -- has to start w/ "Point"
    _ <- P.spaces
    _ <- P.char '{'
    _ <- P.spaces
    x <- parseHex "x" -- x value
    _ <- P.spaces
    y <- parseHex "y" -- y value
    _ <- P.spaces
    _ <- P.char '}'
    return (Types.Point (read(x)) (read(y))) -- call a constructor using the 2

-- function to read a value that contains a hex number
-- ^val\s*:\s*([0-9]|[A-F])+
parseHex :: String -> Str.Parser String
parseHex valName = do
    _ <- P.string valName --first, find the correct value
    _ <- P.spaces -- dont care about number of spaces
    _ <- P.char ':' -- there has to be a ':' somewhere
    _ <- P.spaces
    _ <- P.string "0x" -- hexa number begins with 0x in the input
    givenVal <- P.many1 (P.hexDigit) -- read while the char is a valid hex value
    return ("0x" ++ givenVal)

-- function to parse a value w/ an integer
-- ^val\s*:\s*([0-9])+
parseInt :: String -> Str.Parser String
parseInt valName = do
    _ <- P.string valName --first, find the correct value
    _ <- P.spaces -- dont care about number of spaces
    _ <- P.char ':' -- there has to be a ':' somewhere
    _ <- P.spaces
    givenVal <- P.many1 (P.digit) -- read while the char is a valid hex value
    return givenVal
