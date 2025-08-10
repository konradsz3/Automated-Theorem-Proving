module Main (main) where

import Test.Framework (defaultMain)
import qualified Parser.ParserSpec
import qualified Semantics.SemanticsSpec
import Test.Framework (testGroup)

main :: IO ()
main = defaultMain (Parser.ParserSpec.tests ++ Semantics.SemanticsSpec.tests)