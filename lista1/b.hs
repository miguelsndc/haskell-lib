pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n =
    filter (\(i,j,k) -> i*i +j*j == k*k && i<j && j<k) [(i, j, k) | i <- [1..n], j <- [1..n], k <- [1..n]]

main = do
    s <- getLine
    let result = pitagoras (read s)
    print result