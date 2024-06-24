-- This was done on June 24th 2024 - June 24th_19:19 2024

import System.IO
import Data.Char (isDigit)

storeLinesInArray :: FilePath -> IO [String]
storeLinesInArray path = do
    contents :: String <- readFile path
    return (lines contents)

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

    let temp = removeLettersFromLinesLoop listOfLines
    let onlyLineDigits = duplicateSingleDigitsLoop temp

    let firstAndLastLineDigits = removeMiddleDigitsLoop onlyLineDigits

    let integerLineDigits = turnStringsIntoIntsLoop firstAndLastLineDigits

    let sumOfAllDigits = sum integerLineDigits

    print sumOfAllDigits
