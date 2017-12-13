module DM.CourseworkSecondTable where

import Control.Arrow ((***))
import Text.LaTeX.Base.Syntax

import ReportBase
import DM.Logic

truthTableTeX :: LaTeXM ()
truthTableTeX = environment2 "longtable" [FixArg $ TeXRaw "| c | c c | c | c c | c | c | c c |"] $ do 
  hline
  trow (mt <$> ["a_{sign}", "a_1", "a_2", "b_{sign}", "b_1", "b_2", "c_{carry}", "c_{sign}", "c_1", "c_2"])
  mapM_ id [ makerow [a0, a1, a2] [b0, b1, b2] |
    a0 <- [False, True], b0 <- [False, True], 
    a1 <- [False, True], a2 <- [False, True], b1 <- [False, True], b2 <- [False, True] ]

makerow :: [Bool] -> [Bool] -> LaTeXM ()
makerow as bs = trow (
  (bitstr <$> as) ++ (bitstr <$> bs) ++ [bitstr carry] ++ (bitstr <$> cs))
  where
    (cs, carry) = rippleCarryAdder as bs
    bitstr = fromString . show . bit

rippleCarryAdder :: [Bool] -> [Bool] -> ([Bool], Bool)
rippleCarryAdder as bs = go (reverse as) (reverse bs) [] False
  where
    go [] [] sum carry = (sum, carry)
    go (a:as) (b:bs) sum carry = go as bs (absum : sum) abcarry
      where
        (absum, abcarry) = fullAdder a b carry

fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
fullAdder a b carry = (carry /= (a /= b) , (a && b) || ((a /= b) && carry))

halfAdder :: Bool -> Bool -> (Bool, Bool)
halfAdder a b = (a /= b, a && b)

