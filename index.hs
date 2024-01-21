import System.IO
import Data.Char (toLower)
import Data.List (sort, group)
import qualified Data.Map as Map
import Control.Monad

-- Function to split a string into words
splitWords :: String -> [String]
splitWords = words . map toLower

-- Function to count the frequency of each word
countWords :: [String] -> [(String, Int)]
countWords = Map.toList . Map.fromListWith (+) . flip zip (repeat 1)

-- Function to process the file and analyze text
processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    let wordList = splitWords content
    let wordCount = countWords wordList
    let sortedWordCount = reverse . sort $ wordCount
    forM_ sortedWordCount (print . show)

-- Main function
main :: IO ()
main = do
    putStrLn "Enter the filename to analyze:"
    filename <- getLine
    processFile filename
