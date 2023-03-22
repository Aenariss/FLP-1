{-
Project: FLP 2023 Haskell Project
Author: Vojtech Fiala <xfiala61>
-}

module Types where

-- Curve type definition
data Curve = Curve
    { p :: String
    , a :: Integer
    , b :: Integer
    , g :: Point
    , n :: String
    , h :: Integer
    } deriving (Eq)

-- Point type definition
data Point = Point 
    { x :: String
    , y :: String
    } deriving (Show)

-- Might be useful for points to be comparable, cant compile just w/ deriving Eq
instance Eq Point where
    (Point q w) == (Point u i) = q == u && w == i
