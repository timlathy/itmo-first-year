{-# LANGUAGE FlexibleContexts #-}

module Compiler where

import Data.List (intersect)
import Data.Char (intToDigit, toUpper)
import Control.Monad.State

import qualified Vocabulary as V
import qualified Program as P

data CompilerState = CompilerState { remainingWords :: [String]
                                   , constants :: [String]
                                   , asmCode :: String }
  deriving (Show)

initialState :: String -> CompilerState
initialState code = CompilerState (words code) [] ""

runCompiler :: String -> V.MainVocabularies -> String
runCompiler code vocabs = evalState (compile vocabs) (initialState code)

compile :: V.MainVocabularies -> State CompilerState String
compile (internalv, asmv) = recurse
   where
    recurse = hasWord >>= parse
    parse True = nextWord >>= process >> recurse
    parse False = do
      update (\s -> s { asmCode = P.writeProgram (constants s) (asmCode s) internalv })
      asmCode <$> get
    process w
      | isNumeric w = update (\s -> s
          { constants = constants s ++ [parseNumeric w]
          , asmCode = asmCode s ++ P.pushConstant (length $ constants s) })
      | w == "ORG" = do
          ('0':'x':addr) <- nextWord
          update (\s -> s { asmCode = asmCode s ++ P.setOrgAddr addr })
      | w == "VARIABLE" = do
          name <- nextWord
          update (\s -> s { asmCode = asmCode s ++ P.insertVariable name })
      | otherwise =
          case P.tryAsmWord w asmv of
            Just asmc -> update (\s -> s { asmCode = asmCode s ++ asmc })
            Nothing -> update id
    update f = state (\s -> ((), f s))
    lookahead :: State CompilerState String
    lookahead = do
      ws <- remainingWords <$> get
      (pure $ case ws of
                w:_ -> w
                [] -> "")
    nextWord :: State CompilerState String
    nextWord = do
      (w:ws) <- remainingWords <$> get
      update (\s -> s { remainingWords = ws })
      return w
    hasWord = (/= []) <$> remainingWords <$> get

isNumeric :: String -> Bool
isNumeric ('0':'x':w) = w `intersect` "0123456789ABCDEF" == w
isNumeric w = w `intersect` "0123456789" == w

parseNumeric :: String -> String
parseNumeric ('0':'x':w) = padHex w
parseNumeric w = padHex . decToHex . read $ w
  where
    -- Source: https://stackoverflow.com/a/26333467
    decToHex = (toUpper <$>) . reverse . go
    go n
      | n < 16 = [intToDigit n]
      | otherwise = let (q, r) = n `divMod` 16 in (intToDigit r) : go q

padHex :: String -> String
padHex s = replicate (4 - length s) '0' ++ s
