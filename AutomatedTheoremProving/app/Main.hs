module Main (main) where

import System.IO
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)
import System.IO (readFile)
import Text.Parsec.Error (ParseError)
import Parser 

-- | Main function to parse a file
main :: IO ()
main = parseFile 

parseFile :: IO ()
parseFile = do
  args <- getArgs
  case args of
    [fileName] -> do
      contents <- readFile fileName
      case parseProgram contents of
        Left err -> do
          putStrLn "Parse error:"
          print err
        Right ast -> do
          putStrLn "Successfully parsed program:"
          print ast
    _ -> putStrLn "Usage: parser <filename>"

parseAndPrint :: String -> IO ()
parseAndPrint input = do
  putStrLn "Input:"
  putStrLn input
  putStrLn ""
  putStrLn "Result:"
  case parseProgram input of
    Left err -> print err
    Right ast -> print ast