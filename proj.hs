import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)

data Tree atr thresh = EmptyTree | Node atr thresh (Tree atr thresh) (Tree atr thresh) | Leaf String
    deriving (Show)

-- vstup: [2.4, 1.3] -> Tree atr thresh -> String
-- treeFind vrati listovou hodnotu dle toho, jak se rozhoduje v uzlech
treeFind :: (Ord a, Integral atr) => [a] -> Tree atr a -> String
treeFind _ EmptyTree = "EmptyTree"
treeFind _ (Leaf s) = s
treeFind all@(x:xs) (Node atr thresh leftTree rightTree)
    | fromIntegral atr < 0 || fromIntegral atr >= length all = "Index out of bounds"
    | all!!fromIntegral atr <= thresh = treeFind all leftTree
    | otherwise = treeFind all rightTree

parseLine :: String -> Tree Int Double
parseLine line
    | take 4 line == "Node" = 
        let lineParts = words line
            attr  = read (init lineParts !! 1)  :: Int -- Get the attribute and remove the comma using init
            thresh = read (lineParts !! 2)       :: Double -- Get the threshold
        in Node attr thresh EmptyTree EmptyTree
    | take 4 line == "Leaf" = Leaf (drop 6 line) -- Directly take the substring after "Leaf: "
    | otherwise = EmptyTree

-- parseLine :: String -> (Tree Int Double, String)
-- parseLine line
--   | take 4 line == "Node" =
--       let lineParts = words line
--           attrStr = init (lineParts !! 1)
--           threshStr = lineParts !! 2
--           attr = read attrStr :: Int
--           thresh = read threshStr :: Double
--       in (Node attr thresh EmptyTree EmptyTree, "Parsed Node")
--   | take 4 line == "Leaf" = (Leaf (drop 6 line), "Parsed Leaf")
--   | otherwise = (EmptyTree, "Failed to parse")


-- getLines :: FilePath -> IO ()
-- getLines filePath = do
--     content <- readFile filePath
--     let linesOfContent = lines content
--     mapM_ putStrLn linesOfContent

testParseLine :: IO ()
testParseLine = do
    let nodeLine = "Node: 1, 2.5"
    let leafLine = "Leaf: ClassA"
    print $ parseLine nodeLine
    print $ parseLine leafLine

-- main :: IO ()
-- main = do
--     content <- readFile "file.txt"
--     let linesOfContent = lines content
--     mapM_ (print . parseLine) linesOfContent


leafParser :: Parser (Tree Int Double)
leafParser = spaces >> string "Leaf:" >> spaces >> many1 anyChar >>= return . Leaf

main :: IO ()
main = do
    content <- readFile "file.txt"
    let linesOfContent = lines content
    case parse leafParser "" content of
        Left err -> print err  -- If parsing fails, print the error
        Right tree -> print tree  -- If parsing succeeds, print the resul
    -- mapM_ (\line -> let (tree, message) = parseLine line in print (tree, message)) linesOfContent
