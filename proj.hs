import System.IO
import System.Environment (getArgs)
import ParseFile (parseTree, parseData, parseTraining)
import DataStructures (Tree(..))
import Control.Monad (forM_)

-- vstup: [2.4, 1.3] -> Tree attr thresh -> String
-- treeFind vrati listovou hodnotu dle toho, jak se rozhoduje v uzlech
treeFind :: (Ord a, Integral attr) => [a] -> Tree attr a -> String
treeFind _ EmptyTree = "EmptyTree"
treeFind _ (Leaf s) = s
treeFind all@(x:xs) (Node attr thresh leftTree rightTree)
    | fromIntegral attr < 0 || fromIntegral attr >= length all = "Index out of bounds"
    | all!!fromIntegral attr <= thresh = treeFind all leftTree
    | otherwise = treeFind all rightTree

------------------------------------------------------------------------------------------------

cleanString :: String -> String
cleanString str = filter (`notElem` ['\r', '\"']) str

main :: IO ()
main = do
    -- Get command-line arguments
    args <- getArgs  
    case args of
        [filePath] -> do
            -- Perform parsing based on the file path
            fileContent <- readFile filePath
            let parsedContent = parseTraining fileContent
            case parsedContent of
                Left err -> putStrLn $ "Error parsing file: " ++ show err
                Right result -> do
                    putStrLn $ "Done"
                    print result

        -- when args is a list of two elements, bind the files
        [treeFilePath, dataFilePath] -> do
            -- Read and parse the tree from the tree file
            treeContent <- readFile treeFilePath
            let parsedTree = parseTree treeContent
            
            case parsedTree of
                Left err -> print $ "Error parsing tree: " ++ show err
                Right tree -> do
                    -- If the tree is successfully parsed, proceed to parse the data
                    dataContent <- readFile dataFilePath
                    let dataLines = lines dataContent
                    forM_ dataLines $ \line -> do
                        let parsedData = parseData line
                        case parsedData of
                            Left err -> print $ "Error parsing data: " ++ show err
                            Right dataValues -> do
                                let result = treeFind dataValues tree
                                print $ cleanString result  -- Clean and print the result
        _ -> putStrLn "Usage: program <tree file path> <data file path>"

