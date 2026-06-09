data Time = Haiti | Escocia | Brasil | Marrocos | Catar | Suica | Canada | Bosnia
  deriving (Show, Eq)

type Jogo = (Time, Int, Int, Time)

jogos1 :: [Jogo]
jogos1 =
  [ (Haiti, 1, 3, Escocia),
    (Brasil, 2, 0, Marrocos),
    (Catar, 0, 2, Suica),
    (Canada, 0, 0, Bosnia),
    (Suica, 0, 1, Bosnia),
    (Canada, 0, 0, Catar),
    (Brasil, 1, 0, Haiti),
    (Escocia, 0, 1, Marrocos),
    (Suica, 1, 0, Canada),
    (Bosnia, 1, 1, Catar),
    (Marrocos, 0, 0, Haiti),
    (Escocia, 1, 2, Brasil)
  ]

gols :: Time -> [Jogo] -> Int
gols time jogos = sum (map (\(t1, g1, g2, t2) -> if t1 == time then g1 else if t2 == time then g2 else 0) jogos)

pontos :: Time -> [Jogo] -> Int
pontos time jogos = sum(map dale jogos)
    where 
        dale (t1,g1,g2,t2)
            | t1 == time && g1 > g2 = 3
            | t2 == time && g2 > g1 = 3
            | (t1 == time || t2 == time) && g1 == g2 = 1
            | otherwise = 0

saldo :: Time -> [Jogo] -> Int
saldo time jogos = sum (map dale jogos)
    where 
        dale (t1, g1, g2, t2)
            | t1 == time = g1 - g2
            | t2 == time = g2 - g1
            | otherwise = 0

faz :: Time ->[Jogo] -> (Int, Time)
faz time jogos = (pontos time jogos, time)

ordena :: [(Int, Time)] -> [(Int, Time)]
ordena [] = []
ordena ((n,t):xs) = 
    ordena [(ny,ty) | (ny,ty) <- xs, ny >= n] ++ [(n,t)] ++ ordena [(ny,ty) | (ny,ty) <- xs, ny < n]

classificados :: [Time] -> [Jogo] -> [Time]
classificados times jogos = 
    let resF = map(\t -> faz t jogos) times
        resG = ordena resF
    in take 2 (map snd resG)

