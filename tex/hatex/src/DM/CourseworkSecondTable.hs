module DM.CourseworkSecondTable (renderSopCube, renderCubesInChunksOf, sQuineEq, truthTableTeX) where

import Data.List (intersperse)
import Data.List.Split (chunksOf)
import Control.Arrow ((***))
import Text.LaTeX.Base.Syntax

import ReportBase
import DM.Logic

truthTableTeX :: LaTeXM ()
truthTableTeX = environment2 "longtable" [FixArg $ TeXRaw "| c | c c | c | c c | c | c | c c |"] $ do 
  hline
  trow (mt <$> ["a_{sign}", "a_1", "a_2", "b_{sign}", "b_1", "b_2", "c_{overflow}", "c_{sign}", "c_1", "c_2"])
  mapM_ id [ makerow [a0, a1, a2] [b0, b1, b2] |
    a0 <- [False, True], b0 <- [False, True], 
    a1 <- [False, True], a2 <- [False, True], b1 <- [False, True], b2 <- [False, True] ]

renderCubesInChunksOf :: Int -> String -> [String] -> LaTeXM ()
renderCubesInChunksOf n sep = (mapM_ line) . (chunksOf n)
  where
    line = (<> linebreak) . (alignment <>) . mconcat . intersperse (raw $ fromString sep) . (renderSopCube <$>)
    alignment = raw "&"
    linebreak = raw "\\\\\n"

sQuineEq :: [String] -> LaTeXM ()
sQuineEq cs = withResult . (mconcat . withOps) . ((fromIntegral . sQuineByCube) <$>) $ cs
  where
    withResult = (<> (" = " <> (fromIntegral . sum . (sQuineByCube <$>)) cs))
    withOps = intersperse (raw "+")

sQuineByCube :: String -> Int
sQuineByCube = (+ 1) . sum . ((\v -> if v == 'X' then 0 else 1) <$>)

renderSopCube :: String -> LaTeXM ()
renderSopCube = raw . mconcat . (print <$>) . (zip ["a_{sign}\\text{\\vphantom{b}}", "a_1", "a_2", "b_{sign}", "b_1", "b_2"])
  where
    print (name, 'X') = ""
    print (name, '0') = "\\widebar{" <> name <> "}"
    print (name, '1') = name

makerow :: [Bool] -> [Bool] -> LaTeXM ()
makerow as bs = trow (
  (bitstr <$> as) ++ (bitstr <$> bs) ++ showresult)
  where
    showresult = case add as bs of
                   (bits, False) -> "0" : (bitstr <$> bits)
                   (bits, _overflow) -> "1" : (bitstr <$> bits)
    bitstr = fromString . show . bit

add :: [Bool] -> [Bool] -> ([Bool], Bool)
add (asign:as) (bsign:bs)
  | (bitsToDec as == bitsToDec bs) && (bitsToDec as == 0 || asign /= bsign) =
      (replicate (length as + 1) False, False)
  | asign == bsign =
      ((:) asign) *** id $ (rippleCarryAdder as bs)
  | bitsToDec as > bitsToDec bs =
      (asign : ((fst . (rippleCarryAdder as) . twosComplement) bs), False)
  | otherwise =
      (bsign : ((fst . (rippleCarryAdder bs) . twosComplement) as), False)

twosComplement :: [Bool] -> [Bool]
twosComplement n = fst $ rippleCarryAdder (flip <$> n) one
  where
    flip True = False
    flip False = True
    one = (replicate (length n - 1) False) ++ [True]

rippleCarryAdder :: [Bool] -> [Bool] -> ([Bool], Bool)
rippleCarryAdder as bs = go (reverse as) (reverse bs) [] False
  where
    go [] [] sum carry = (sum, carry)
    go (a:as) (b:bs) sum carry = go as bs (absum : sum) abcarry
      where
        (absum, abcarry) = fullAdder a b carry

fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
fullAdder a b carry = (carry /= (a /= b) , (a && b) || ((a /= b) && carry))
