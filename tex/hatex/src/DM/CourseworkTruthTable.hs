{-# LANGUAGE ScopedTypeVariables #-}

module DM.CourseworkTruthTable (truthTable, truthTableTeX) where

import ReportBase
import DM.Logic

-- f=1 when 3 <= (x_1x_2x_3+x_4x_5) < 7
-- f=d when (x_3x_4) = 2

sourceFun :: [Bool] -> BoolFuncValue
sourceFun = go . verboseSourceFun
  where
    go (_, _, _, _, v) = v

verboseSourceFun :: [Bool] -> ([Bool], [Bool], [Bool], Int, BoolFuncValue)
verboseSourceFun [x1, x2, x3, x4, x5] =
  (addend1, addend2, dcTerm, decSum, funcValue)
  where
    addend1 = [x1, x2, x3]
    addend2 = [x4, x5]
    decSum = (bitsToDec addend1) + (bitsToDec addend2)
    dcTerm = [x3, x4]
    funcValue
      | dcTerm == [True, False]   = D
      | 3 <= decSum && decSum < 7 = T
      | otherwise                 = F

bits :: [Bool]
bits = [False, True]

truthTable :: BoolTruthTable
truthTable = [ let args = [x1, x2, x3, x4, x5] in (args, sourceFun args) |
  x1 <- bits, x2 <- bits, x3 <- bits, x4 <- bits, x5 <- bits ]

truthTableTeX :: LaTeXM ()
truthTableTeX =
  borderedtable [(CenterColumn, 10)] $ do
    -- Header
    hline
    trow [ mt "N", mt "x_1x_2x_3x_4x_5"
         , mt "x_1x_2x_3", mt "{(x_1x_2x_3)}_{10}", mt "x_4x_5", mt "{(x_4x_5)}_{10}"
         , mt "x_3x_4",  mt "{(x_3x_4)}_{10}", mt "(+)", mt "f" ]
    -- Contents
    mconcat $ zipWith (\(i :: Int) r -> (fromString . show) i & r) [0..]
      [ truthTableRow [x1, x2, x3, x4, x5] |
          x1 <- bits, x2 <- bits, x3 <- bits, x4 <- bits, x5 <- bits ]

truthTableRow :: [Bool] -> LaTeXM ()
truthTableRow args =
  trow [ fromString $ (show . bit) =<< args
       , fromString $ (show . bit) =<< addend1
       , fromString $ (show . bitsToDec) addend1
       , fromString $ (show . bit) =<< addend2
       , fromString $ (show . bitsToDec) addend2
       , fromString $ (show . bit) =<< dcTerm
       , fromString $ (show . bitsToDec) dcTerm
       , fromString $ show decSum
       , fromString $ show funcValue ]
  where
    (addend1, addend2, dcTerm, decSum, funcValue) = verboseSourceFun args
