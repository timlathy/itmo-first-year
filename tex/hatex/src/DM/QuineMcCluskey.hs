module DM.QuineMcCluskey where

import DM.Logic

data CubeValue = Bound Int | Unbound
  deriving (Show)

zeroCubes :: [BoolTerm] -> [CubeValue]
zeroCubes = (go =<<)
  where
    go (And terms) = unwrap <$> terms
    unwrap (X _) = Bound 1
    unwrap (Not (X _)) = Bound 0
