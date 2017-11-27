module Main where

import Compiler
import qualified Vocabulary as V

import qualified System.IO.Strict as S

main :: IO ()
main = S.getContents >>= compileString >>= putStrLn

compileString :: String -> IO String
compileString s = V.loadMainVocabs >>= return . runCompiler s
