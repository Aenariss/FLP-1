{-
Project: FLP 2023 Haskell Project
File: Types.hs
Author: Vojtech Fiala <xfiala61>
-}

module Types where

import Text.Printf as Printf
-- Curve type definition
data Curve = Curve
    { p :: Integer
    , a :: Integer
    , b :: Integer
    , g :: Point
    , n :: Integer
    , h :: Integer
    } deriving (Eq)

-- format the show instance to match the expected output
instance Show Curve where
    show (Curve q w e r t u) = "Curve {\n" ++ "p: "++ (Printf.printf "0x%X" q) ++ "\na: " ++ show w ++ "\nb: " ++ show e ++ "\ng: " ++ show r ++ "\nn: " ++ (Printf.printf "0x%X" t) ++ "\nh: " ++ show u ++ "\n}"

-- Point type definition
data Point = Point 
    { x :: Integer
    , y :: Integer
    }

-- format the show to match the expected
instance Show Point where
    show (Point q w) = "Point {\n    x: " ++ (Printf.printf "0x%X" q) ++ "\n    y: " ++ (Printf.printf "0x%X" w) ++ "\n}"

-- Might be useful for points to be comparable, cant compile just w/ deriving Eq
instance Eq Point where
    (Point q w) == (Point u i) = q == u && w == i
