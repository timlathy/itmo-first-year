module Binary where

bitwiseAnd :: [Int] -> [Int] -> [Int]
bitwiseAnd as bs = bitand <$> (zip (pad as) (pad bs))
  where
    pad ns = padding ns ++ ns
    padding ns = take (longest - length ns) (repeat 0)
    longest = max (length as) (length bs)
    bitand (a, b) = if a + b == 2 then 1 else 0

decToBits :: Int -> [Int]
decToBits = reverse . (go [])
  where
    go rems 0 = 0 : rems
    go rems 1 = 1 : rems
    go rems n = (n `rem` 2) : rems ++ (go rems (n `div` 2))

bitsToDec :: [Int] -> Int
bitsToDec = (foldr digitToPower 0) . (zip powers) . reverse
  where
    digitToPower (pow, d) = (+ d * 2^pow)

powers :: [Int]
powers = [0..]
