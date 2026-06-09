votos :: [Int]
votos = [1, 2, 1, 9, 99, 9, 8, 2, 2, 9, 1, 7, 0, 1, 8, 8, 8, 8, 7]

unique :: [Int] -> [Int]
unique [] = []
unique (x : xs) = x : unique [y | y <- xs, y /= x]

freq :: Int -> [Int] -> Int
freq c [] = 0
freq c (x : xs)
  | c == x = 1 + freq c xs
  | otherwise = freq c xs

winner :: [Int] -> Int
winner cand = head $ maior $ unique cand
  where
    maior [] = []
    maior (x : xs) =
      [y | y <- xs, freq y cand >= freq x cand]
        ++ [x]
        ++ [y | y <- xs, freq y cand < freq x cand]