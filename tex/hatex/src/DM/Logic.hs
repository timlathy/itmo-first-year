module DM.Logic where

data BoolTerm = And [BoolTerm]
              | Or [BoolTerm]
              | Not BoolTerm
              | X Int -- refers to the variable position in the input set; starts from 1

evalTerm :: [Bool] -> BoolTerm -> Bool
evalTerm inputs (X i) = inputs !! (i - 1)
evalTerm inputs (Not t) = not $ evalTerm inputs t
evalTerm inputs (Or ts) = or $ (evalTerm inputs) <$> ts
evalTerm inputs (And ts) = and $ (evalTerm inputs) <$> ts
