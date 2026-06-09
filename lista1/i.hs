factor :: Int -> [(Int, Int)]
factor n = go n 2
  where
    go 1 _ = []
    go n d
      | d * d > n = [(n, 1)]
      | n `mod` d == 0 =
          let (cnt, resto) = count n d 0
           in (d, cnt) : go resto (d + 1)
      | otherwise = go n (d + 1)
    count n d c
      | n `mod` d == 0 = count (n `div` d) d (c + 1)
      | otherwise = (c, n)

main = do
  a <- getLine
  let result = factor (read a :: Int)
  print result