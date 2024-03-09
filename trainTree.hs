import DataStructures (DataSet, ValueFromDataset)
import Data.List (nub, group, sort)

{-|
  Function: createHalfs
  Description: Calculates averages between next and prev. number in dataset.

  Parameters:
    - paramName1: xs: A list of (Double, String) pairs to process.

  Returns:  A list of (Double, Double) pairs, where the first Double is the average
            of one column elements from the input dataset, and the second Double is 0.0.
-}
createHalfs :: [(Double, String)] -> [(Double, Double)]
createHalfs []  = []
createHalfs [_] = []  -- list has only one elem
createHalfs xs  = zipWith avg (init xs) (tail xs)
    where avg (a, _) (b, _) = ( (a + b)/2, 0.0 )


-----------------------------------------------------------------------

{-|
  Function: getClasses, getClassesFromDataset
  Description: Extracts a list of unique class names from the last column of a 2D list.

  Parameters:
    - DataSet: A list of (Double, String) pairs to process.

  Returns:  A list of (Double, Double) pairs, where the first Double is the average
            of one column elements from the input dataset, and the second Double is 0.0.
-}
getClassesFromDataset :: (Eq a) => [a] -> [a]
getClassesFromDataset [] = []
getClassesFromDataset (x:xs)
    | elem x xs = getClassesFromDataset xs
    | otherwise = x : getClassesFromDataset xs

getClasses :: DataSet -> [ValueFromDataset]
getClasses xs = getClassesFromDataset (map last xs)

-----------------------------------------------------------------------

-- LeftNode a RightNode vytvoreni. Ke kazde tride mas 0 [("A", 0), ("B", 0), ...]
-- a bude se to inkrementovat podle zastoupeni
createNodes :: [ValueFromDataset] -> [(ValueFromDataset, Integer)]
createNodes classes = map (\x -> (x, 0)) classes

-- Inkrementace tridy v danem podstromu
-- incrementClass :: (Eq String) => String -> [(String, Integer)] -> [(String, Integer)]
-- incrementClass targetClass pairs = map updatePair pairs
--   where
--     updatePair (cls, num) = if cls == targetClass then (cls, num + 1) else (cls, num)



{-
    Parameters:
        - a: index of row in dataset
        - a: number to be compared to
        - [ValueFromDataset]: column of dataset - doubles
    Returns:
        - [(a, Integer)]: Pairs of classes and it's representations
-}
-- Prvne vytvor dva podstromy - leftNode a rightNode
-- Pak zavolej funkci calcGini, ktere predas hodnotu na i. radku z datasetu a compareHodnotu (polovina)
-- a ta na zaklade toho vraci 
-- 1. param = index
-- 2. param = compare number
-- 3. param = sloupec
-- return List dvojic (trida, giniIndex)
-- calcNumFromDataset :: Integer -> Double -> [ValueFromDataset] -> [(String, Integer)]
-- calcNumFromDataset _ _ [] = []
-- calcNumFromDataset index compareNumber (x:xs) =
--   let 



-- Count occurrences of each class
countClasses :: [String] -> [(String, Integer)]
countClasses classes = map (\grp -> (head grp, toInteger $ length grp)) . group . sort $ classes



-- [(7, "No"), (12, "No"), (18, "Yes"), (35, "Yes"), (38, "Yes"), (50, "No"), (83, "No")]


{-
- Vypocet total gini impurity pro threshold.
    Vstupy:
      [(Double, String)]  =   (hodnota, trida)
      Double              =   threshold 
    
    Vystup:
      Gini impurity pro threshold.
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
- Vypocet gini impurity pro jeden list.
    Vstupy:
      [(String, Integer)]  = (trida, pocet zastoupeni)
    
    Vystup:
      Gini impurity pro list.
-}
calcLeafImpurity :: [(String, Integer)] -> Double
calcLeafImpurity pair = 
  let listOfClasses = map snd pair -- [0, 1] zastoupeni trid
      numberOfClasses = fromIntegral $ sum listOfClasses -- Convert to Double for floating-point division

      classesProbabilities = map (\x -> (fromIntegral x / numberOfClasses) ** 2) listOfClasses 
      sumOfProbabilities = sum classesProbabilities
      impurity = 1 - sumOfProbabilities
  in impurity