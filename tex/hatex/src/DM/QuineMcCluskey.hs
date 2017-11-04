module DM.QuineMcCluskey where

import Data.List ((\\), union, sortBy)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.LaTeX (Render, render, (<>), fromString)

import DM.Logic
import qualified Binary as B

--
-- Types
--

type CubeLabel = Int

data Cube = Cube [CubeLabel] (Seq CubeValue)
  deriving (Show)

data CubeValue = Bound Bool | Unbound
  deriving (Show)

instance Render Cube where
  render (Cube ls vals) = (mconcat . toList) (render <$> vals) <>
    " (" <> fromString (show =<< ls) <> ")"

instance Render CubeValue where
  render (Bound True) = "1"
  render (Bound False) = "0"
  render (Unbound) = "\\textbf{X}"

--
-- Functions
--

zeroCubes :: [BoolTerm] -> [Cube]
zeroCubes = (cube <$>)
  where
    cube (And terms) = Cube 
      [bitsToDec $ unwrap <$> terms]
      (Seq.fromList $ (Bound . unwrap) <$> terms)
    unwrap (X _) = True
    unwrap (Not (X _)) = False

cubeDisplaySort :: [Cube] -> [Cube]
cubeDisplaySort = sortBy (compare `on` (\(Cube _ vals) -> B.bitsToDec . toList $ valBit <$> vals))
  where
    valBit (Bound True) = 1
    valBit (_) = 0

combineCubes :: [Cube] -> [Cube]
combineCubes cs = [ combine c1 c2 | c1 <- cs, c2 <- cs,
    equalUnbound c1 c2, let ds = boundDiffs c1 c2 in length ds == 1 ]
  where
    combine c1@(Cube l1 s1) c2@(Cube l2 _) =
      let [d] = boundDiffs c1 c2
      in Cube (l1 ++ l2) (Seq.update d Unbound s1)
    boundDiffs :: Cube -> Cube -> [Int]
    boundDiffs (Cube _ s1) (Cube _ s2) =
      (Seq.findIndicesL is1 s1) `diff` (Seq.findIndicesL is1 s2)
      where
        diff a b = (a \\ b) `union` (b \\ a)
    equalUnbound (Cube _ s1) (Cube _ s2) =
      (Seq.findIndicesL isUnbound s1) == (Seq.findIndicesL isUnbound s2)
    isUnbound Unbound = True
    isUnbound _ = False
    is1 (Bound True) = True
    is1 _ = False
