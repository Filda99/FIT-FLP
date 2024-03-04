module ParseFile (parseTree, parseData) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<*>), (<$>))
import DataStructures (Tree(..))

----------------------------------------------------------------------

-- Parser for attributes (assuming they are integers)
attrParser :: Parser Integer
attrParser = read <$> many1 digit

-- Parser for thresholds (assuming they are doubles)
threshParser :: Parser Double
threshParser = read <$> (many1 digit <* char '.' <* many1 digit)

-- Parser for leaf nodes, allowing spaces before "Leaf"
leafParser :: Parser (Tree attr thresh)
leafParser = Leaf <$> (spaces *> string "Leaf: " *> many1 (noneOf "\n"))

-- Parser for node constructs, allowing variable spaces before "Node"
nodeParser :: Parser (Tree Integer Double)
nodeParser = Node <$> (spaces *> string "Node: " *> attrParser <* char ',' <* spaces)
                  <*> (threshParser <* spaces)
                  <*> (spaces *> treeParser)
                  <*> (spaces *> treeParser)

-- Parser for the tree structure
treeParser :: Parser (Tree Integer Double)
treeParser = try nodeParser <|> leafParser <|> return EmptyTree

-- Top-level parser to parse the entire input
parseTree :: String -> Either ParseError (Tree Integer Double)
parseTree input = parse (treeParser <* eof) "" input

----------------------------------------------------------------------

numberParser :: Parser Double
numberParser = read <$> (many1 digit <* char '.' <* many1 digit)

dataParser :: Parser [Double]
dataParser = numberParser `sepBy` char ','

parseData :: String -> Either ParseError [Double]
parseData input = parse (dataParser <* eof) "" input