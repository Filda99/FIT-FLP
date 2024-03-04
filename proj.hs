import System.IO
import ParseFile (parseTree, parseData)
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

main :: IO ()
main = do
    -- Reading and parsing the tree from tree.txt
    treeContent <- readFile "tree.txt"
    let parsedTree = parseTree treeContent
    
    case parsedTree of
        Left err -> print $ "Error parsing tree: " ++ show err
        Right tree -> do
            -- If the tree is successfully parsed, proceed to parse the data
            dataContent <- readFile "data.txt"
            let dataLines = lines dataContent  -- Split the content into lines
            forM_ dataLines $ \line -> do
                let parsedData = parseData line  -- Parse each line individually
                case parsedData of
                    Left err -> print $ "Error parsing data: " ++ show err
                    Right dataValues -> do
                        -- Using the treeFind function with the parsed data and tree
                        let result = treeFind dataValues tree
                        putStrLn $ filter (`notElem` "\"\r") result

