{-# LANGUAGE ScopedTypeVariables #-}

module DM.CourseworkTruthTable (truthTable, truthTableTeX) where

import ReportBase
import DM.Logic

-- f=1 when 9 < (1x_4x_5 + x_1x_2x_3) <= 12
-- f=d when |x_5x_1x_2 - x_4x_3| is an element of {0, 5}

sourceFun :: [Bool] -> BoolFuncValue
sourceFun = go . verboseSourceFun
  where
    go (_, _, _, _, _, _, v) = v

verboseSourceFun :: [Bool] -> ([Bool], [Bool], Int, [Bool], [Bool], Int, BoolFuncValue)
verboseSourceFun [x1, x2, x3, x4, x5] =
  (addend1, addend2, decSum, minuend, subtrahend, decDiff, funcValue)
  where
    addend1 = [True, x4, x5]
    addend2 = [x1, x2, x3]
    decSum = (bitsToDec addend1) + (bitsToDec addend2)
    minuend = [x5, x1, x2]
    subtrahend = [x4, x3]
    decDiff = abs $ (bitsToDec minuend) - (bitsToDec subtrahend)
    funcValue
      | decDiff `elem` [0, 5]      = D
      | 9 < decSum && decSum <= 12 = T
      | otherwise                  = F

bits :: [Bool]
bits = [False, True]

truthTable :: [([Bool], BoolFuncValue)]
truthTable = [ let args = [x1, x2, x3, x4, x5] in (args, sourceFun args) |
  x1 <- bits, x2 <- bits, x3 <- bits, x4 <- bits, x5 <- bits ]

truthTableTeX :: LaTeXM ()
truthTableTeX = do
  raw "\\resizebox{\\textwidth}{!}{"
  borderedtable [(CenterColumn, 13)] $ do
    -- Header
    hline >>
      mt "N" & mt "x_1x_2x_3x_4x_5" & mt "1x_4x_5" & mt "{(1x_4x_5)}_{10}" &
      mt "x_1x_2x_3" & mt "{(x_1x_2x_3)}_{10}" & mt "(+)" &
      mt "x_5x_1x_2" & mt "{(x_5x_1x_2)}_{10}" & mt "x_4x_3" & mt "{(x_4x_3)}_{10}" & mt "|-|" & mt "f" >>
      lnbk >> hline
    -- Contents
    mconcat $ zipWith (\(i :: Int) r -> (fromString . show) i & r) [0..]
      [ truthTableRow [x1, x2, x3, x4, x5] |
          x1 <- bits, x2 <- bits, x3 <- bits, x4 <- bits, x5 <- bits ]
  raw "}"

truthTableRow :: [Bool] -> LaTeXM ()
truthTableRow args =
  trow [ fromString $ (show . bit) =<< args
       , fromString $ (show . bit) =<< addend1
       , fromString $ (show . bitsToDec) addend1
       , fromString $ (show . bit) =<< addend2
       , fromString $ (show . bitsToDec) addend2
       , fromString $ show decSum
       , fromString $ (show . bit) =<< minuend
       , fromString $ (show . bitsToDec) minuend
       , fromString $ (show . bit) =<< subtrahend
       , fromString $ (show . bitsToDec) subtrahend
       , fromString $ show decDiff
       , fromString $ show funcValue ]
  where
    (addend1, addend2, decSum, minuend, subtrahend, decDiff, funcValue) = verboseSourceFun args
