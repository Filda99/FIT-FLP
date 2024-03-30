import System.IO
import System.Environment (getArgs)
import ParseFile (parseTree, parseData, parseTraining)
import DataStructures (Tree(..))
import TrainTree(buildTree)

{-
 * File: proj.hs
 * Author: Filip Jahn
 * Login: xjahnf00
 * Email: xjahnf00@stud.fit.vutbr.cz
 * Date: 2024-03-30
 * Description: Implementation of the main module.
                Arguments parsing and processing.
-}

------------------------------------------------------------------------------------------------


{-
Tree find function will recursively traverse the tree based on the given data.

    Inputs:
        [Double] - List of doubles which represents the data.
        Tree - Decision tree with nodes and leaves.

    Returns:
        String - The class name which is found in the tree.
-}
treeFind :: (Ord a, Integral attr, Num a) => [a] -> Tree attr a -> String
treeFind _ EmptyTree = "EmptyTree"
treeFind _ (Leaf s) = s
treeFind doubles (Node attr thresh leftTree rightTree)
    | null doubles = "Empty list"  -- Handling the case where the list is empty
    | attr' < 0 || attr' >= length doubles = "Index out of bounds"
    | doubles !! attr' <= thresh = treeFind doubles leftTree
    | otherwise = treeFind doubles rightTree
    where
        attr' = fromIntegral attr


------------------------------------------------------------------------------------------------

cleanString :: String -> String
cleanString str = filter (`notElem` ['\r', '\"']) str


main :: IO ()
main = do
    args <- getArgs
    case args of
        [_, filePath] -> trainMode filePath
        [_, treeFilePath, dataFilePath] -> classifyMode treeFilePath dataFilePath
        _ -> putStrLn "Usage of the program:\n\
        \   flp-fun -1 <file with a tree> <file with new data>\n\
        \   flp-fun -2 <file with training data>\n\
        \"

{- 
Process a single file (training data).

Inputs: 
  filePath: Path to the file to be processed.

Outputs: 
  None (performs IO actions - printing the trained tree).
-}
trainMode :: String -> IO ()
trainMode filePath = 
    -- Read the content of the file.
    readFile filePath >>= \fileContent ->
    -- Parse the content of the file.
    let parsedContent = parseTraining fileContent
    in case parsedContent of
        -- If parsing fails, print an error message.
        Left err -> putStrLn $ "Error parsing file: " ++ show err
        -- If parsing succeeds, build a tree and print the result.
        Right trainingData ->
            let result = buildTree trainingData
            in print result

{- 
Process multiple files (when tree and data are in separate files).
At first process the tree file, build a tree and save it to internal representation.
Then process the data file, parse the data and find the class in the tree.
Then print the cleaned result which are the classes for data provided based on 
the tree.

Inputs: 
  treeFilePath: Path to the file containing tree data.
  dataFilePath: Path to the file containing data to be processed.
Outputs: 
  None (performs IO actions - print the classes for the data from the data file).
-}
classifyMode ::String -> String -> IO ()
classifyMode treeFilePath dataFilePath =
    -- Read the content of the tree file.
    readFile treeFilePath >>= \treeContent ->
    -- Parse the content of the tree file.
    let parsedTree = parseTree treeContent
    in case parsedTree of
        -- If parsing fails, print an error message.
        Left err -> print $ "Error parsing tree: " ++ show err
        -- If parsing succeeds, process the data file.
        Right tree ->
            -- Read the content of the data file.
            {-
            The result of IO action is passed to the function on the right side
            as an argument. 
            -}
            readFile dataFilePath >>= \dataContent ->
            -- Split the data file into lines.
            let dataLines = lines dataContent
            -- Process each line of data.
            {-
            mapM_ is a monadic version of map. It applies the function to each element/line.
            From the list (dataLines) take line by line and apply the function on the right side.
            Performs the finding of the class in the tree and prints the cleaned result.
            -}
            in mapM_ (\line ->
                -- Parse the data line.
                let parsedData = parseData line
                in case parsedData of
                    -- If parsing fails, print an error message.
                    Left err -> print $ "Error parsing data: " ++ show err
                    -- If parsing succeeds, find data in the tree and print the cleaned result.
                    Right dataValues ->
                        let result = treeFind dataValues tree
                        in print $ cleanString result
            ) dataLines
