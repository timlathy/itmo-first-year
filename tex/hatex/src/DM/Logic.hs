{-# LANGUAGE LambdaCase #-}

module DM.Logic where

import Data.List (intersperse)
import Text.LaTeX (Render, render, (<>))

--
-- Types
--

data BoolTerm = And [BoolTerm]
              | Or [BoolTerm]
              | Not BoolTerm
              | X Int -- refers to the variable position in the input set; starts from 1
  deriving (Show)

data BoolFuncValue = T | F | D
  deriving (Eq)

type BoolTruthTable = [([Bool], BoolFuncValue)]

-- ! This code assumes @LaTeXHelpers.defineWidebar@ is included in the document.
instance Render BoolTerm where
  render (And [term]) = render term
  render (And terms) = mconcat $ (flip fmap) terms (\case
    var@(X _) -> render var
    negvar@(Not (X _)) -> render negvar
    term -> "(" <> render term <> ")")
  render (Or terms) = mconcat $ intersperse " \\lor " (render <$> terms)
  render (Not term) = "\\widebar{" <> render term <> "}"
  render (X i) = "x_" <> render i

instance Show BoolFuncValue where
  show T = "1"
  show F = "0"
  show D = "d"

--
-- Canonical normal forms
--

sumOfProducts :: BoolTruthTable -> BoolTerm
sumOfProducts = Or . minterms

productOfSums :: BoolTruthTable -> BoolTerm
productOfSums = And . maxterms

minterms :: BoolTruthTable -> [BoolTerm]
minterms = ((And . argsToVarterms) <$>) . (selectArgs (== T))

maxterms :: BoolTruthTable -> [BoolTerm]
maxterms = ((Or . argsToVarterms) <$>) . (selectArgs (== F))

dontcareminterms :: BoolTruthTable -> [BoolTerm]
dontcareminterms = ((Or . argsToVarterms) <$>) . (selectArgs (== D))

selectArgs :: (BoolFuncValue -> Bool) -> BoolTruthTable -> [[Bool]]
selectArgs p = (fst <$>) . (filter (p . snd))

argsToVarterms :: [Bool] -> [BoolTerm]
argsToVarterms = zipWith complementTerm [1..]
  where
    complementTerm i True = X i
    complementTerm i False = Not $ X i

--
-- Utils
--

evalTerm :: [Bool] -> BoolTerm -> Bool
evalTerm inputs (X i) = inputs !! (i - 1)
evalTerm inputs (Not t) = not $ evalTerm inputs t
evalTerm inputs (Or ts) = or $ (evalTerm inputs) <$> ts
evalTerm inputs (And ts) = and $ (evalTerm inputs) <$> ts

bit :: Bool -> Int
bit True = 1
bit False = 0

bitsToDec :: [Bool] -> Int
bitsToDec = (go 0 0) . reverse
  where
    go :: Int -> Int -> [Bool] -> Int
    go _ num [] = num
    go weight num (b:bs) = go (weight + 1) (num + ((bit b) * 2 ^ weight)) bs
