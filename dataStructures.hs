module DataStructures (Tree(..), DataSet, ValueFromDataset(..), Row) where

data Tree attr thresh = 
    EmptyTree 
    | Node attr thresh (Tree attr thresh) (Tree attr thresh) 
    | Leaf String

instance (Show attr, Show thresh) => Show (Tree attr thresh) where
    show tree = showTree' tree 0
      where
        showTree' EmptyTree _ = ""
        showTree' (Leaf s) indent = replicate indent ' ' ++ "Leaf: " ++ s ++ "\n"
        showTree' (Node attr thresh left right) indent =
            replicate indent ' ' ++ "Node: " ++ show attr ++ ", " ++ show thresh ++ "\n" ++
            showTree' left (indent + 2) ++
            showTree' right (indent + 2)  

----------------------------------------------------

data ValueFromDataset = 
    Numeric Double 
    | Label String
    deriving (Show, Eq)

type Row = [ValueFromDataset]
type DataSet = [Row]