logMes :: String -> String -> Double
logMes mes s = foldl (+) 0 [read v | [d, _, v] <- group3 (split s), mes `elem` words d]
  where
    split "" = []
    split s = let (h, t) = break (==';') s in h : if null t then [] else split (tail t)
    group3 (a:b:c:xs) = [a,b,c] : group3 xs
    group3 _ = []

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result