module ParseFile (parseTree, parseData) where

import Text.Parsec
import Text.Parsec.String (Parser)
import DataStructures (Tree(..))

----------------------------------------------------------------------

-- Parser for attributes 
integerParser :: Parser Integer
-- Consists of many digits, then convert to Integer
integerParser = read <$> many1 digit

-- Parser for thresholds
{- 
    At first parse the integral part, then concate it with the rest when it is done.
    The rest parse the decimal point and then add it as a head to the list. Tail is the 
    fractional part then. At the end convert it to the double. 
-}
doubleParser :: Parser Double
doubleParser = read <$> ((++) <$> many1 digit <*> ((:) <$> char '.' <*> many1 digit))

----------------------------------------------------------------------

-- Parser for leaf nodes, allowing spaces before "Leaf"
leafParser :: Parser (Tree Integer Double)
leafParser = Leaf <$> (spaces *> string "Leaf: " *> many1 (noneOf "\n"))

-- Parser for node constructs, allowing variable spaces before "Node"
nodeParser :: Parser (Tree Integer Double)
nodeParser = Node <$> (spaces *> string "Node: " *> integerParser <* char ',' <* spaces)
                  <*> (doubleParser <* spaces)
                  <*> (spaces *> treeParser)
                  <*> (spaces *> treeParser)

-- Parser for the tree structure
treeParser :: Parser (Tree Integer Double)
treeParser = try nodeParser <|> leafParser <|> return EmptyTree

-- Top-level parser to parse the entire input
parseTree :: String -> Either ParseError (Tree Integer Double)
parseTree input = parse (treeParser <* eof) "" input

----------------------------------------------------------------------

dataParser :: Parser [Double]
dataParser = doubleParser `sepBy` char ','

parseData :: String -> Either ParseError [Double]
parseData input = parse (dataParser <* eof) "" input