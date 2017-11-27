{-# LANGUAGE FlexibleContexts #-}

module Compiler where

import Data.List (intersect)
import Data.Char (intToDigit, toUpper)
import Control.Monad (forM_)
import Control.Monad.State

import qualified Vocabulary as V
import qualified Program as P

data CompilerState = CompilerState { inputBase :: InputBase
                                   , constants :: [String]
                                   , asmCode :: String }
  deriving (Show)

data InputBase = DecimalInput | HexInput
  deriving (Show)

initialState :: CompilerState
initialState = CompilerState DecimalInput [] ""

runCompiler :: String -> V.MainVocabularies -> String
runCompiler code vocabs = evalState (compile code vocabs) initialState

compile :: String -> V.MainVocabularies -> State CompilerState String
compile code (internalv, asmv) = do
  forM_ (words code) handleWord
  update (\s -> s { asmCode = P.writeProgram (constants s) (asmCode s) internalv })
  (flip fmap) get asmCode
  where
    handleWord w = process w =<< (inputBase <$> get)
    process w base
      | isNumeric w base = update (\s -> s
        { constants = (constants s) ++ [parseNumeric w base]
        , asmCode = (asmCode s) ++ P.pushConstant (length $ constants s) })
      | w == "DECIMAL"   = update (\s -> s { inputBase = DecimalInput })
      | w == "HEX"       = update (\s -> s { inputBase = HexInput })
      | otherwise =
          case P.tryAsmWord w asmv of
            Just asmc -> update (\s -> s { asmCode = asmCode s ++ asmc })
            Nothing -> update id
    update f = state (\s -> ((), f s))

isNumeric :: String -> InputBase -> Bool
isNumeric w DecimalInput = w `intersect` "0123456789" == w
isNumeric w HexInput = w `intersect` "0123456789ABCDEF" == w

parseNumeric :: String -> InputBase -> String
parseNumeric w HexInput = padHex w
parseNumeric w DecimalInput = padHex . decToHex . read $ w
  where
    -- Source: https://stackoverflow.com/a/26333467
    decToHex = (toUpper <$>) . reverse . go
    go n
      | n < 16 = [intToDigit n]
      | otherwise = let (q, r) = n `divMod` 16
                    in (intToDigit r) : go q

padHex :: String -> String
padHex s = replicate (4 - length s) '0' ++ s
