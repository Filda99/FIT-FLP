module TrainTree(buildTree) where

{-
 * File: trainTree.hs
 * Author: Filip Jahn
 * Login: xjahnf00
 * Email: xjahnf00@stud.fit.vutbr.cz
 * Date: 2024-03-30
 * Description: Implementation of training decision tree.
-}


import DataStructures (DataSet, ValueFromDataset(..), Tree (..), Row)
import Data.List (group, sort, sortBy, transpose, minimumBy)
import Data.Function (on)


{-
Build tree function will calculate gini index for it's dataset, then
it will split the dataset into two parts and recursively call itself.

  Inputs:
    DataSet - 2D field which contains:
                - Doubles - [Numeric Double]
                - Classes - [Label String]

  Returns:
    Tree - Decision tree with nodes and leaves.

  Notes:
    For each node, the dataset is split into two parts based on the threshold
    and attribute. The threshold is calculated based on the smallest gini index.
    The attribute is the index of the column with the smallest gini index.
    The dataset is split into two parts based on the threshold and attribute.
    The function is recursive and will continue to split the dataset until
    all classes are the same.
-}
buildTree :: DataSet -> Tree Int Double
buildTree dataset =
  let 
    (thresh, attr, gini) = getGiniForDataset dataset
    -- Get all class names from the last column
    classesNames = countClasses [s | Label s <- last (transpose dataset)]
  in 
    if length classesNames == 1 
    then Leaf (fst (head classesNames)) 
    else 
      let 
        leftLeafClasses  = filter (\ row -> ( (\(Numeric x) -> x) (row !! attr) ) < thresh) dataset
        rightLeafClasses = filter (\ row -> ( (\(Numeric x) -> x) (row !! attr) ) >= thresh) dataset
      in Node attr thresh (buildTree leftLeafClasses) (buildTree rightLeafClasses)


{-
For given dataset calculate and return the smallest gini impurity with the threshold that will be used.

  Inputs:
    DataSet  - 2D field which contains:
                  - Doubles - [Numeric Double]
                  - Classes - [Label String]  
    
  Returns:
    Smallest pair for given dataset where fst is Threshold and snd is Column Index = Attribute. 

  Notes:
    For any other node but initial the parameter dataset should be adjusted based on
    node before (only those rows which meets the condition - threshold).
    This function is little bit complex so every line is greatly commented because i will
    forget the thought process... :)
-}
getGiniForDataset :: DataSet -> (Double, Int, Double)
getGiniForDataset dataset =
  let 
      -- Extract class labels from last column
      -- Using transpose we can work with columns
      classLabels = [s | Label s <- last (transpose dataset)]
      
      -- Get only numerical columns from dataset 
      -- (excluding the classes - they will be added to each numerical column)
      numericalColumns = init (transpose dataset)

      -- For each column with doubles, extract the double values
      extractDoubles col = [x | Numeric x <- col]

      -- Pair each doubles column with classes
      pairedColumns = [(extractDoubles col, classLabels) | col <- numericalColumns]

      -- Calculate Gini for each column, storing result and assign an index to each pair.
      -- 1. getGiniForColumn takes list of pairs (value, class) so we need at first zip
      -- all doubles from column with the class
      -- 2. getGiniForColumn is applied to created list of touples and returns list of
      -- pairs (giniIndex, thresh). 
      -- 3. Add attribute (index of column) to the touples.
      allGinis = zip [0..] (map (getGiniForColumn . (\(a,b) -> zip a b)) pairedColumns)

      -- Find the column with the smallest Gini coefficient
      (columnIndex, (_, thresh)) = minimumBy (compare `on` snd . snd) allGinis

      -- Sort gini indexes by gini index ascending
      sortedGinis = sortBy (\(_, (giniA, _)) (_, (giniB, _)) -> compare giniA giniB) allGinis

      -- Get the smallest gini index and return only threshold and attribute
      (colIdFinal, (gini, threshFinal)) = head sortedGinis 

  -- in (threshFinal, colIdFinal)
  in (threshFinal, colIdFinal, gini)


{-
Helping function for getGiniForColumn.
Function will create candidates for threshold for the given column.

  Inputs:
    [(Double, String)]  - rows from dataset (value from dataset, class) 
    
  Returns:
    List of candidates (element count is count of input rows - 1).
-}
createHalfs :: [(Double, String)] -> [Double]
createHalfs []  = []
createHalfs [_] = []  -- list has only one elem
createHalfs xs  = zipWith avg (init xs) (tail xs)
    where avg (a, _) (b, _) = (a + b) / 2


{-
For column calculate and return the smallest gini impurity.

  Inputs:
    [(Double, String)]  - rows from dataset (value from dataset, class) 
    
  Returns:
    Smallest pair for this column where fst is Gini impurity and snd is threshold. 
-}
getGiniForColumn :: [(Double, String)] -> (Double, Double)
getGiniForColumn pairs =
 
  let 
      -- Create candidates for thresholds but first sort it
      sortedPairs = sortBy (\(a,_) (b,_) -> compare a b) pairs
      candidates = createHalfs sortedPairs
      -- Create a list of gini indexes
      listGinis = [(calcThreshImpurity pairs candidate, candidate) | candidate <- candidates]
      -- Sort gini indexes by gini index ascending
      sortedGinis = sortBy (\(a,_) (b,_) -> compare a b) listGinis
      -- Get the smallest gini index and return it
      initGini = head sortedGinis

  in initGini


{-
Create pair (class, count) for left and right subtree.
At first, sort classes so the same are next each other.
Then group them - it will create a list of lists, where in each
sublist there are only the same classes.
In the end create a pair where first elem will be class name
and the second elem will be count of elements in sublist.

  Inputs:
    [String] - Classes
    
  Returns:
    [(String, Integer)]   =  (class, count) 
-}
countClasses :: [String] -> [(String, Integer)]
countClasses classes = map (\grp -> (head grp, toInteger $ length grp)) . group . sort $ classes


{-
Calculation of total gini impurity for selected threshold.

  Inputs:
    [(Double, String)]  - rows from dataset (value from dataset, class) 
    Double              - threshold 
    
  Returns:
    Gini impurity for threshold.
-}
calcThreshImpurity :: [(Double, String)] -> Double -> Double
calcThreshImpurity pairs threshold =
  -- Based on the value in the pair (compared to the threshold), 
  -- create two lists (left and right) with only classes
  let leftLeafClasses = [cls | (val, cls) <- pairs, val < threshold]
      rightLeafClasses = [cls | (val, cls) <- pairs, val >= threshold]
      leftLeafNoDuplicates = countClasses leftLeafClasses
      rightLeafNoDuplicates = countClasses rightLeafClasses

      leftGini = calcLeafImpurity leftLeafNoDuplicates
      rightGini = calcLeafImpurity rightLeafNoDuplicates

      totalLeft = sum [cnt | (_, cnt) <- leftLeafNoDuplicates]
      totalRight = sum [cnt | (_, cnt) <- rightLeafNoDuplicates]

      totalCountOfClasses = fromIntegral (totalLeft + totalRight)

      totalGini = (fromIntegral totalLeft / totalCountOfClasses) * leftGini +
                  (fromIntegral totalRight / totalCountOfClasses) * rightGini
  in  totalGini


{-
Calculation of Gini impurity for one leaf.

    Inputs:
      [(String, Integer)] - rows from dataset fulfilling the condition (class, count)
    
    Returns:
      Gini impurity for a leaf.
-}
calcLeafImpurity :: [(String, Integer)] -> Double
calcLeafImpurity pair =
  let listOfClasses = map snd pair -- [0, 1] for example
      numberOfClasses = fromIntegral $ sum listOfClasses -- Convert to Double for floating-point division

      classesProbabilities = map (\x -> (fromIntegral x / numberOfClasses) ** 2) listOfClasses
      sumOfProbabilities = sum classesProbabilities
      impurity = 1 - sumOfProbabilities
  in impurity