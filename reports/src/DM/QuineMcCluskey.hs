{-# LANGUAGE ScopedTypeVariables #-} 

module DM.QuineMcCluskey where

import Control.Arrow ((&&&))
import Control.Monad (forM_, when)
import Data.Text (pack, unpack)
import Data.Tuple (swap)
import Data.List ((\\), union, sortBy, groupBy, nubBy, intersperse, transpose)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.LaTeX.Base.Class (fromLaTeX)
import Text.LaTeX.Base.Syntax (LaTeX (..), MathType (..))

import DM.Logic
import DM.CourseworkTruthTable
import qualified Binary as B
import ReportBase

import Debug.Trace

--
-- Types
--

type CubeCombination = (Int, Int)
type CubeCoverage = (Bool, Bool) -- (covered, included in Z)

data Cube = Cube [CubeCombination] CubeCoverage (Seq CubeValue)
  deriving (Eq)

data CubeValue = Bound Bool | Unbound
  deriving (Eq)

instance Show Cube where
  show (Cube ls _ vals) =
    ((mconcat . intersperse ("-") . (show <$>)) ls) <> " " <> mconcat (show <$> (toList vals))

instance Show CubeValue where
  show (Bound True) = "1"
  show (Bound False) = "0"
  show (Unbound) = "X"

instance Render Cube where
  render (Cube ls (cov, inc) vals) = highlight (values) <> coverage <> "\\quad" <> labels
    where
      highlight tex = if inc then "\\textbf{" <> tex <> "}" else tex
      coverage = if cov then " $\\checkmark$\\hfill" else "\\hfill"
      values = (mconcat . toList) (render <$> vals)
      labels = (fromString . mconcat . (intersperse " ") . (label <$>)) ls
      label (c1, c2) = show c1 <> "-" <> show c2
  
instance Render CubeValue where
  render = fromString . show

--
-- Functions
--

defaultCoverage :: CubeCoverage
defaultCoverage = (False, False)

zeroCubes :: [BoolTerm] -> [Cube]
zeroCubes = (cube <$>)
  where
    cube (And terms) = Cube [] defaultCoverage (Seq.fromList $ (Bound . unwrap) <$> terms)
    cube (Or terms) = Cube [] defaultCoverage (Seq.fromList $ (Bound . unwrap) <$> terms)
    unwrap (X _) = True
    unwrap (Not (X _)) = False

numOfOnes :: Cube -> Int
numOfOnes (Cube _ _ vs) = length . (filter ((==) (Bound True))) . toList $ vs

numOfZeroes :: Cube -> Int
numOfZeroes (Cube _ _ vs) = length . (filter ((==) (Bound False))) . toList $ vs

sortOnValues :: (Cube -> Int) -> [Cube] -> [Cube]
sortOnValues pred = sortBy p
  where
    p a b = ((compare `on` pred) a b) `mappend` ((compare `on` decValue) a b)
    decValue = B.bitsToDec . (valBit <$>) . vallist
    vallist (Cube _ _ vs) = toList vs
    valBit (Bound True) = 1
    valBit (_) = 0

groupOnValues :: (Cube -> Int) -> [Cube] -> [[Cube]]
groupOnValues p = groupBy ((==) `on` p)

combineCubes :: [Cube] -> [Cube]
combineCubes cs = [ combine (c1, i1) (c2, i2) |
    (c1, i1) <- (zip cs [1..]),
    (c2, i2) <- (zip cs [1..]),
    equalUnbound c1 c2,
    let ds = boundDiffs c1 c2 in length ds == 1 ]
  where
    combine (c1@(Cube _ _ s1), i1) (c2@(Cube _ _ _), i2) =
      let [d] = boundDiffs c1 c2
      in Cube [(i1, i2)] defaultCoverage (Seq.update d Unbound s1)
    boundDiffs :: Cube -> Cube -> [Int]
    boundDiffs (Cube _ _ s1) (Cube _ _ s2) =
      (Seq.findIndicesL is1 s1) `diff` (Seq.findIndicesL is1 s2)
      where
        diff a b = (a \\ b) `union` (b \\ a)
    equalUnbound (Cube _ _ s1) (Cube _ _ s2) =
      (Seq.findIndicesL isUnbound s1) == (Seq.findIndicesL isUnbound s2)
    isUnbound Unbound = True
    isUnbound _ = False
    is1 (Bound True) = True
    is1 _ = False

combineToMaxCubes :: (Cube -> Int) -> [Cube] -> [[Cube]]
combineToMaxCubes sortpred = reverse . go . pure
  where
    go :: [[Cube]] -> [[Cube]]
    go ([]:maxcubes) = maxcubes
    go acc@(prev:_) = go ((combined prev) : acc)
    combined = dumbJoin . (nubBy dedupe) . (sortOnValues sortpred) . combineCubes
    dedupe (Cube [c1] _ _) (Cube [c2] _ _) = (swap c1) == c2
    dedupe _ _ = False
    dumbJoin = go []
      where
        go uniq [] = uniq
        go uniq ((c@(Cube _ _ srcvs)):cs) =
          case filter (\(Cube _ _ vs) -> vs == srcvs) cs of
            [] -> go (uniq ++ [c]) cs
            same -> go (uniq ++ [merge (c : same)]) (cs \\ same)
        merge cubes@((Cube _ _ vs):_) = Cube combs defaultCoverage vs
          where
            combs = cubes >>= (\(Cube cbs _ _) -> cbs) 

insertCubeGroupBreaks :: [[Cube]] -> ([Cube], [Int])
insertCubeGroupBreaks = (go ([], []))
  where
    go acc [] = acc
    go (acc, []) (x:xs) = go (acc ++ x, [length x]) xs
    go (acc, (breaks@(b:_))) (x:xs) = go (acc ++ x, ((length x) + b) : breaks) xs

determineCoverage :: [[Cube]] -> [[Cube]]
determineCoverage = (go []) . reverse
  where
    go acc [] = acc
    go [] (max:cs) = go [(\(Cube ls _ vs) -> Cube ls (False, True) vs) <$> max] cs
    go (acc@(higheroc:_)) (c:cs) = go ((mark <$> zip c [1..]) : acc) cs
      where
        mark ((Cube ls _ vs), cubei) = Cube ls (covered cubei, not (covered cubei)) vs
        covered i = i `elem` coveredIndices
        coveredIndices = ((\(a, b) -> [a, b]) =<<) =<< (\(Cube ls _ _) -> ls) <$> hoccovering
        hoccovering = filter (\(Cube _ (cov, inc) _) -> cov || inc) higheroc 

cubeTable :: (BoolTruthTable -> [[Cube]]) -> (Cube -> Int) -> LaTeXM ()
cubeTable groupingf sortingf = centerbox $ do
  borderedtable [(LeftColumn, maxcube + 1)] $ do
    hline
    trow [mt "K^0(f) \\cup N(f)", mt "K^1(f)", mt "K^2(f)", mt "K^3(f)", mt "Z(f)"]
    forM_ (zip rows [1..]) tablerow
    hline
  where
    tablerow (cells, rowi) = tfreerow (fromLaTeX . (number rowi) <$> cells) <> mconcat (rowborders rowi)
    rowborders i = (flip fmap) (zip breaks [1..]) (\(bs, ci) -> if i `elem` bs then (cline ci ci) else mempty)
    number i (TeXRaw rc) = TeXRaw $ (pack $ show i) <> ". " <> rc
    number _ r = r
    breaks = snd <$> table
    rows :: [[LaTeX]] = (transpose . (balance <$>)) cols
    balance col = if length col < maxcollen
                    then col ++ (replicate (maxcollen - (length col)) mempty)
                    else col
    maxcollen = maximum (length <$> cols)
    cols :: [[LaTeX]] = (insertNotes . (((rendertex <$>) . fst) <$>)) $ table
    insertNotes [c1, c2, c3, c4] = [c1, c2, c3, c4 ++ [TeXEmpty, TeXSeq TeXEmpty $ (raw "$K^4 = \\varnothing$")], zcubestex]
    zcubestex = (TeXSeq TeXEmpty) <$> rendertex <$> reverse (zcubes (groupingf truthTable))
    table :: [([Cube], [Int])] = insertCubeGroupBreaks <$> ((groupOnValues sortingf) <$> (groupingf truthTable))
    maxcube = length (groupingf truthTable)

cubegroups :: BoolTruthTable -> [[Cube]]
cubegroups = determineCoverage . (combineToMaxCubes numOfOnes) .
  (sortOnValues numOfOnes) . zeroCubes .
  (uncurry (++)) . (minterms &&& dontcareminterms)

cubegroupspos :: BoolTruthTable -> [[Cube]]
cubegroupspos = determineCoverage . (combineToMaxCubes numOfZeroes) .
  (sortOnValues numOfZeroes) . zeroCubes .
  (uncurry (++)) . (maxterms &&& dontcaremaxterms)

zcubes :: [[Cube]] -> [Cube]
zcubes = (eraseLabel <$>) . (filter included) . concat
  where
    eraseLabel (Cube _ cov vs) = Cube [] cov vs
    included (Cube _ (_, inc) _) = inc

implicantTable :: (BoolTruthTable -> [BoolTerm]) -> (BoolTruthTable -> [[Cube]]) -> LaTeXM ()
implicantTable termf cubef =
  borderedtable [(LeftColumn, (length tms) + 1)] $ do
    hline
    trow ("Простые импликанты" : ((raw . renderTerms) <$> tms))
    forM_ zcbs renderImplicant
  where
    renderImplicant c = trow $ (rendertex c) : ((\t -> if coversTerm c t then "*" else "") <$> tms)
    --wraprow c t = if singleCovering c then raw "\\rowcolor{gray!60}" <> t else t
    singleCovering c = or (coversTerm c <$> singleCoverage)
    singleCoverage = (fst <$>) $ filter snd $ zip tms $ (\c -> length (filter id c) == 1) <$>
      (transpose ((\c -> (coversTerm c) <$> tms) <$> zcbs))
    renderTerms (And vals) = "\\makecell{" <> mconcat (intersperse " \\\\ " (renderx <$> vals)) <> "}"
    renderTerms (Or vals) = "\\makecell{" <> mconcat (intersperse " \\\\ " (renderx <$> vals)) <> "}"
    renderx (X _) = "1"
    renderx (Not (X _)) = "0"
    zcbs = reverse $ zcubes $ cubef truthTable
    tms = termf truthTable

coversTerm :: Cube -> BoolTerm -> Bool
coversTerm (Cube _ _ vals) terms = match (serialize <$> extract terms) (toList $ show <$> vals)
  where
    match [] [] = True
    match (_:ts) ("X":vs) = match ts vs
    match ("1":ts) ("1":vs) = match ts vs
    match ("0":ts) ("0":vs) = match ts vs
    match _ _ = False
    serialize (X _) = "1"
    serialize (Not (X _)) = "0"
    extract (And ts) = ts
    extract (Or ts) = ts
