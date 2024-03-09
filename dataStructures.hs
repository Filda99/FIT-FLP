{-# LANGUAGE DerivingStrategies #-} -- Allow specifying the strategy for each derived instance explicitly

module DataStructures (Tree(..), DataSet, ValueFromDataset(..)) where

data Tree attr thresh = 
    EmptyTree 
    | Node attr thresh (Tree attr thresh) (Tree attr thresh) 
    | Leaf String
    deriving (Show)

----------------------------------------------------

data ValueFromDataset = 
    Numeric Double 
    | Label String
    deriving (Show, Eq)
    

type Row = [ValueFromDataset]
type DataSet = [Row]

