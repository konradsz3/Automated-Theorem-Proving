module Main (main) where

import System.IO
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lib
import Parser

main :: IO ()
main = do
  setLocaleEncoding utf8 
