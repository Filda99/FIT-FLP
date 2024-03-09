import DataStructures (DataSet, ValueFromDataset)
import Data.List (nub, group, sort, sortBy)



-- [(7, "No"), (12, "No"), (18, "Yes"), (35, "Yes"), (38, "Yes"), (50, "No"), (83, "No")]



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
  -- Vytvor kandidaty na thresh
  let candidates = createHalfs pairs
      -- Vytvor list s gini indexy
      listGinis = [(calcThreshImpurity pairs candidate, candidate) | candidate <- candidates]
      sortedGinis = sortBy (\(a,_) (b,_) -> compare a b) listGinis
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
  -- Na zaklade hodnoty v pair (porovnani s threshold) vytvor dva 
  -- listy (left a right), ve kterych budou jenom tridy
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
  let listOfClasses = map snd pair -- [0, 1] zastoupeni trid
      numberOfClasses = fromIntegral $ sum listOfClasses -- Convert to Double for floating-point division

      classesProbabilities = map (\x -> (fromIntegral x / numberOfClasses) ** 2) listOfClasses 
      sumOfProbabilities = sum classesProbabilities
      impurity = 1 - sumOfProbabilities
  in impurity