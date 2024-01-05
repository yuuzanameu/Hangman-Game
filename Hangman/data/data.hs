
import Data.Char(toLower)
import Data.List

-- data Progress = Progress
--                 {  guessWord    :: String,
--                    validGuesses :: [Char],
--                    attempts     :: Int
--                 }


rmvApstphe :: [String] ->  [String]
rmvApstphe [] = []
rmvApstphe (x:xs)
  | length x < 4             = rmvApstphe xs
  | (reverse x) !! 1 == '\'' = (take (length x - 2) x) : rmvApstphe xs
  | otherwise                = x : rmvApstphe xs
 

-- correctguesses :: Progress -> String
-- correctguesses (Progress word guesses _) = go word
--  where
--     go []       = []
--     go (w : ws) = if w `elem` guesses then w : go ws else '_' : go ws

  
main :: IO()
main = do
    file <- readFile "words.txt"
    let newfile = map toLower
                . concat 
                . intersperse "\n" 
                $ lines file 
    appendFile "o.txt" newfile
    print "Done, Anaki"


-- filterSetA_ByExclusivity :: Ord a => [a] -> [a] -> [a]
-- filterSetA_ByExclusivity [] = []
-- filterSetA_ByExclusivity (a : setA) setB
--   | a `elem` setB = a : filterSetA_ByExclusivity setA setB
--   | otherwise     = '_' : filterSetA_ByExclusivity setA setB

-- filterSetA_ByCommonElements :: Ord a => [a] -> [a] -> [a]
-- filterSetA_ByCommonElements [] = []
-- filterSetA_ByCommonElements (a : setA) setB
--   | a not `elem` setB = a : filterSetA_ByCommonElements setA setB
--   | otherwise     = '_'   : filterSetA_ByCommonElements setA setB