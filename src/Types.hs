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
    show (Key private public) = "Key {\nd: " ++ (Printf.printf "0x%x" private) ++ "\nQ: " ++ id public ++ "\n}"

data Hash = Hash 
    { h' :: Integer 
    } deriving (Eq)

instance Show Hash where
    show (Hash hash) = "Hash: " ++ (Printf.printf "0x%x" hash)


data Signature = Signature 
    { r :: Integer
    , s :: Integer
    } deriving (Eq)

instance Show Signature where
    show (Signature r'' s'') = "Signature {\nr: " ++ (Printf.printf "0x%x" r'') ++ "\ns: " ++ (Printf.printf "0x%x" s'') ++ "\n}"

data Sstruct = Sstruct 
    { c''' :: Curve
    , k''' :: Key
    , h''' :: Hash
    }

instance Show Sstruct where
    show (Sstruct cS kS hS) = show cS ++ "\n" ++ show kS ++ "\n" ++ show hS

data Vstruct = Vstruct 
    { c'''' :: Curve
    , s'''' :: Signature
    , pkey' :: String
    , h'''' :: Hash
    }

instance Show Vstruct where
    show (Vstruct cur sig pkey hhash) = show cur ++ "\n" ++ show sig ++ "\n" ++ "PublicKey {\nQ: " ++ id pkey ++ "\n}" ++ "\n" ++ show hhash
