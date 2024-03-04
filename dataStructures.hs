module DataStructures (Tree(..)) where

data Tree attr thresh = 
    EmptyTree 
    | Node attr thresh (Tree attr thresh) (Tree attr thresh) 
    | Leaf String
    deriving (Show)