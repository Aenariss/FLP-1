{-
Project: FLP 2023 Haskell Project
File: flp22-fun.hs
Author: Vojtech Fiala <xfiala61>
-}

module Math where

import qualified Types

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
        m1 = ((3*xp*xp + a) * (pow (2*yp) (p-2) p)) 
        xr1 = (m1^(2::Integer) - xp - xq) `mod` p
        m2 = ((yq - yp) * (pow (xq - xp) (p-2) p))
        xr2 = (m2^(2::Integer) - xp - xq) `mod` p
