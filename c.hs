rankTime :: String -> String -> [(String, Double)]
rankTime target s = sort [ (name, rnd (kda (read k) (read d) (read a))) 
                         | [t, name, k, d, a] <- group5 (split s), t == target ]
  where
    split "" = []
    split s = let (v, r) = break (==';') s in v : if null r then [] else split (tail r)
    
    group5 (a:b:c:d:e:xs) = [a,b,c,d,e] : group5 xs
    group5 _ = []
    
    kda k d a = if d == 0 then k + a else (k + a) / d
    rnd x = fromIntegral (round (x * 100)) / 100
    
    sort [] = []
    sort (x:xs) = sort [y | y <- xs, snd y > snd x] ++ [x] ++ sort [y | y <- xs, snd y <= snd x]

main = do
    time <- getLine
    s <- getLine
    print (rankTime time s)