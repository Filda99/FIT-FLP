module DataStructures (Tree(..), DataSet, ValueFromDataset(..), Row) where

{-
 * File: dataStructures.hs
 * Author: Filip Jahn
 * Login: xjahnf00
 * Email: xjahnf00@stud.fit.vutbr.cz
 * Date: 2024-03-30
 * Description: Data structures needed for an implementation.
-}

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