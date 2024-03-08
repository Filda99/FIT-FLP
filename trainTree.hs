import DataStructures (DataSet, ValueFromDataset)

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


countClasses :: [a] -> Integer
countClasses [] = 0
countClasses (x:xs) = 1 + countClasses xs

getClassesFromDataset :: (Eq a) => [a] -> [a]
getClassesFromDataset [] = []
getClassesFromDataset (x:xs)
    | elem x xs = getClassesFromDataset xs
    | otherwise = x : getClassesFromDataset xs

{-|
  Function: getClasses
  Description: Extracts a list of unique class names from the last column of a 2D list.

  Parameters:
    - paramName1: xs: A list of (Double, String) pairs to process.

  Returns:  A list of (Double, Double) pairs, where the first Double is the average
            of one column elements from the input dataset, and the second Double is 0.0.
-}
getClasses :: DataSet -> [ValueFromDataset]
getClasses xs = getClassesFromDataset (map last xs)