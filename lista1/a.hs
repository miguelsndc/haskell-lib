qsort :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]
qsort [] = []
qsort (x:xs) =
    qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]

ordenaAlunos :: [(String, Int)] -> [(String, Int)]
ordenaAlunos = qsort

main :: IO ()
main = do
    a <- getLine
    let result = ordenaAlunos (read a :: [(String, Int)])
    print result