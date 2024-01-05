{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use when" #-}
module Main (main) where


import Data.List ( intersperse )
import System.Random (randomRIO)
import Data.Char(toUpper)
import System.Exit(exitSuccess)
import Paths_Hangman (getDataFileName)
import System.Process (callCommand)


data Progress = Progress
                {  guessWord    :: String,
                   validGuesses :: [Char],
                   allGuesses   :: [Char]
                }


instance Show Progress where
    show :: Progress -> String
    show pgrss@(Progress word _ allGuess) = "\nThe word is: " ++ map toUpper word ++ "\nCorrect guesses: \n\n" ++ correctguesses pgrss ++ "\n\nAttempts left = " ++ show (15 - length allGuess)


alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"


correctguesses :: Progress -> String
correctguesses (Progress word guesses _) = intersperse ' ' . intersperse ' ' . map toUpper $ go word
 where
    go []       = []
    go (w : ws) = if w `elem` guesses then w : go ws else '_' : go ws


validGuess :: String -> String -> String
validGuess word guesses = go word
 where
    go []       = []
    go (w : ws) = if w `elem` guesses then w : go ws else go ws


allWords :: IO [String]
allWords = do
    file <- getDataFileName "data/words.txt" 
    wordz <- readFile file
    return (lines wordz)


randomWord :: IO String
randomWord = do
    wordz <- allWords
    n <- randomRIO (0, length wordz - 1)
    return (wordz !! n)


availableGuesses :: Progress -> String
availableGuesses (Progress _ _ allG) = map toUpper . intersperse ' ' $ go alphabet
 where
    go []       = []
    go (w : ws) = if w `elem` allG then go ws else w : go ws


handleGuess :: Char -> Progress -> IO Progress
handleGuess x pgrss@(Progress word validG allG) = do
    case (x `elem` allG, x `elem` word) of
        (True, _)  -> do putStrLn $ "Guess already used = "  
                          ++ show [toUpper x] ++ " Attempts left = " 
                          ++ show (14 - length allG) ++ "\n"
                         return pgrss
        (_, True)  -> do putStrLn $ "Right guess, Attempts left = "
                          ++ show (14 - length allG) ++ "\n"
                         return (Progress word (validGuess word (x:validG)) (x:allG))
        (_, False) -> do putStrLn $ "Wrong guess, Attempts left = "
                          ++ show (14 - length allG) ++ "\n"
                         return (Progress word validG (x:allG))


gameWin :: Progress -> IO()
gameWin prgss@(Progress word validG _) = do
    if validG == word
    then do
        putStrLn "Hooray, you've won!! ^\\(*^*)//^\n"
        putStrLn $ correctguesses prgss ++ "\n"
        putStrLn $ "The word was: " ++ map toUpper word
        exitSuccess
    else return ()


gameOver :: Progress -> IO()
gameOver (Progress word _ allG) = do
    if length allG > 14
    then do
        putStrLn "Well that sucks, you've lost (;  ;)\n"
        putStrLn $ "The word was:  " ++ map toUpper word
        exitSuccess
    else return ()


runGame :: Progress -> IO()
runGame game = do
    gameWin game
    gameOver game
    putStrLn $ correctguesses game ++ "\n"
    putStrLn $ availableGuesses game ++ "\n"
    putStrLn "Enter single character: \n\n"
    putStrLn "_____________________________________"
    guess <- getLine
    _ <- callCommand "cls"
    putStrLn $ "Current guess = " ++ show guess
    case guess of
        [c] -> do new_game <- handleGuess c game
                  runGame new_game
        _   -> print "Enter single character"


main :: IO ()
main = do
    putStr "__________________GAME STARTS________________\n\n\n"
    putStr "Guess the Word:\n\n\n"
    word <- randomWord
    runGame (Progress word [] [])
    print "GAME END"
    _ <- getLine
    return ()

