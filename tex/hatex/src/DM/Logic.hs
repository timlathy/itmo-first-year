module DM.Logic where

--
-- Types
--

data BoolTerm = And [BoolTerm]
              | Or [BoolTerm]
              | Not BoolTerm
              | X Int -- refers to the variable position in the input set; starts from 1

data BoolFuncValue = T | F | D

instance Show BoolFuncValue where
  show T = "1"
  show F = "0"
  show D = "d"

--
-- Functions
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
