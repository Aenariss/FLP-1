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
    show (Curve q1 w1 e1 r1 t1 u1) = "Curve {\n" ++ "p: "++ (Printf.printf "0x%X" q1) ++ "\na: " ++ show w1 ++ "\nb: " ++ show e1 ++ "\ng: " ++ show r1 ++ "\nn: " ++ (Printf.printf "0x%X" t1) ++ "\nh: " ++ show u1 ++ "\n}"

-- Point type definition
data Point = Point 
    { x :: Integer
    , y :: Integer
    }

-- format the show to match the expected
instance Show Point where
    show (Point x1 y1) = "Point {\n    x: " ++ (Printf.printf "0x%X" x1) ++ "\n    y: " ++ (Printf.printf "0x%X" y1) ++ "\n}"

-- Might be useful for points to be comparable, cant compile just w/ deriving Eq
instance Eq Point where
    (Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2

data Key = Key 
    { d :: Integer
    , q :: String
    } deriving (Eq)

instance Show Key where
    show (Key private public) = "Key {\n    d: " ++ (Printf.printf "0x%x" private) ++ "\n    Q: " ++ id public ++ "\n}"

