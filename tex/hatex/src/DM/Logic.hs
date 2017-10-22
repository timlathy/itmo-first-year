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
-- Functions
--

sumOfProducts :: [([Bool], BoolFuncValue)] -> BoolTerm
sumOfProducts = Or . (minterms . fst <$>) . filter ((== T) . snd)
  where
    minterms = And . (zipWith (\i truthy -> if truthy then X i else Not (X i)) [1..])

productOfSums :: [([Bool], BoolFuncValue)] -> BoolTerm
productOfSums = And . (maxterms . fst <$>) . filter ((== F) . snd)
  where
    maxterms = Or . (zipWith (\i falsy -> if falsy then X i else Not (X i)) [1..])

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
