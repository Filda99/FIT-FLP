module ParseFile (parseTree, parseData, parseTraining) where

{-
 * File: parseFile.hs
 * Author: Filip Jahn
 * Login: xjahnf00
 * Email: xjahnf00@stud.fit.vutbr.cz
 * Date: 2024-03-30
 * Description: Implementation of parsers for the input files.
                Parsers for:
                - Tree structure
                - Data for classification
                - Training data
-}

import Text.Parsec
import Text.Parsec.String (Parser)
import DataStructures (Tree(..), DataSet, ValueFromDataset(..))
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Text.Parsec()
import Control.Monad.IO.Class()


----------------------------------------------------------------------
-- Integer and Double parsers
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
doubleParser = try scientificNotation <|> try negativeDouble <|> simpleDouble
  where
    simpleDouble :: Parser Double
    simpleDouble = read <$> ((++) <$> integerPart <*> option "" fractionalPart)
      where
        integerPart = (:) <$> option ' ' (char '-') <*> many1 digit
        fractionalPart = (:) <$> char '.' <*> many1 digit
    
    negativeDouble :: Parser Double
    negativeDouble = char '-' *> (negate <$> simpleDouble)
    
    scientificNotation :: Parser Double
    scientificNotation =
        (\sign mantissa decimals exponent' ->
            let result = read mantissa * (10.0 ** fromIntegral (exponent' :: Int) + read decimals / (10.0 ** fromIntegral (length decimals :: Int)))
            in if sign == '-' then negate result else result
        )
        <$> option ' ' (char '-')
        <*> (many1 digit <* char '.')
        <*> many digit
        <*  char 'e'
        <*> (read <$> ((++) <$> option "" (string "-") <*> many1 digit))


----------------------------------------------------------------------
-- Tree parsers
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
treeParser = skipMany (skipMany1 newline *> spaces) *> (try nodeParser <|> leafParser <|> return EmptyTree)
  

-- Top-level parser to parse the entire tree input
parseTree :: String -> Either ParseError (Tree Integer Double)
parseTree input = parse (treeParser <* (skipMany (skipMany1 newline *> spaces)) <* eof) "" input


----------------------------------------------------------------------
-- Data parsers
----------------------------------------------------------------------

-- Parser for the tree structure
dataParser :: Parser [Double]
dataParser = skipMany (skipMany1 newline *> spaces) *> (doubleParser `sepBy` char ',')


-- Top-level parser to parse the entire classification input
parseData :: String -> Either ParseError [Double]
parseData input = parse (dataParser <* (skipMany (skipMany1 newline *> spaces)) <* eof) "" input


----------------------------------------------------------------------
-- Training data parsers
----------------------------------------------------------------------

-- Define the Parser for ValueFromDataset
valueParser :: Parser ValueFromDataset
valueParser = try doubleNumeric <|> myLabel
  where
    doubleNumeric :: Parser ValueFromDataset
    doubleNumeric =
      doubleParser >>= \num ->
      optionMaybe (lookAhead (char ',')) >>= \hasComma ->
      case hasComma of
        Just _  -> return (Numeric num)
        Nothing -> myLabel

    myLabel :: Parser ValueFromDataset
    myLabel = Label <$> many1 (noneOf ",\n")


-- Define the row parser
rowParser :: Parser [ValueFromDataset]
rowParser = valueParser `sepBy` char ','


-- Parse the dataset
datasetParser :: Parser [[ValueFromDataset]]
datasetParser = endBy rowParser newline <* optional newline -- Adjusted to consume extra newlines


-- Top-level parser to parse the entire training input
parseTraining :: String -> Either ParseError DataSet
parseTraining input = parse (skipMany (skipMany1 newline *> spaces) *> datasetParser <* eof) "" input