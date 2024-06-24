-- This was done on June 24th_19:35 2024 - ???

import System.IO
import Data.Char (isDigit)
import Data.List (isPrefixOf)

storeLinesInArray :: FilePath -> IO [String]
storeLinesInArray path = do
    contents :: String <- readFile path
    return (lines contents)

checkForNumberLetters :: String -> String
checkForNumberLetters str@(x:xs)
    | "one" `isPrefixOf` str = '1' : checkForNumberLetters (drop 3 str)
    | "two" `isPrefixOf` str = '2' : checkForNumberLetters (drop 3 str)
    | "three" `isPrefixOf` str = '3' : checkForNumberLetters (drop 5 str)
    | "four" `isPrefixOf` str = '4' : checkForNumberLetters (drop 4 str)
    | "five" `isPrefixOf` str = '5' : checkForNumberLetters (drop 4 str)
    | "six" `isPrefixOf` str = '6' : checkForNumberLetters (drop 3 str)
    | "seven" `isPrefixOf` str = '7' : checkForNumberLetters (drop 5 str)
    | "eight" `isPrefixOf` str = '8' : checkForNumberLetters (drop 5 str)
    | "nine" `isPrefixOf` str = '9' : checkForNumberLetters (drop 4 str)
    | "zero" `isPrefixOf` str = '0' : checkForNumberLetters (drop 4 str)
    | otherwise = x : checkForNumberLetters xs


removeLettersFromLinesLogic :: String -> String
removeLettersFromLinesLogic = filter isDigit

removeLettersFromLinesLoop :: [String] -> [String]
removeLettersFromLinesLoop [] = []
removeLettersFromLinesLoop (x:xs) = removeLettersFromLinesLogic x : removeLettersFromLinesLoop xs

duplicateSingleDigitsLogic :: String -> String
duplicateSingleDigitsLogic str = duplicateIfSingleDigit str
    where
      duplicateIfSingleDigit :: String -> String
      duplicateIfSingleDigit s
        | s `elem` [show n | n <- [0..9]] = concat (replicate 2 s)
        | otherwise = s

duplicateSingleDigitsLoop :: [String] -> [String]
duplicateSingleDigitsLoop []  = []
duplicateSingleDigitsLoop (x:xs) = duplicateSingleDigitsLogic x : duplicateSingleDigitsLoop xs

removeMiddleDigitsLogic :: String -> String
removeMiddleDigitsLogic (x:xs) = x : [last xs]

removeMiddleDigitsLoop :: [String] -> [String]
removeMiddleDigitsLoop [] = []
removeMiddleDigitsLoop (x:xs) = removeMiddleDigitsLogic x : removeMiddleDigitsLoop xs

turnStringsIntoIntsLogic :: String -> Int
turnStringsIntoIntsLogic str = read str :: Int

turnStringsIntoIntsLoop :: [String] -> [Int]
turnStringsIntoIntsLoop [] = []
turnStringsIntoIntsLoop (x:xs) = turnStringsIntoIntsLogic x : turnStringsIntoIntsLoop xs

main :: IO ()
main = do
    listOfLines <- storeLinesInArray "dayOneData.txt"

    let noNumberLetters = map checkForNumberLetters listOfLines

    mapM_ print noNumberLetters

    let temp = removeLettersFromLinesLoop listOfLines
    let onlyLineDigits = duplicateSingleDigitsLoop temp

    let firstAndLastLineDigits = removeMiddleDigitsLoop onlyLineDigits

    let integerLineDigits = turnStringsIntoIntsLoop firstAndLastLineDigits

    let sumOfAllDigits = sum integerLineDigits

    print sumOfAllDigits
