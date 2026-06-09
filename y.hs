data ExpB = OR ExpB ExpB | AND ExpB ExpB | NOT ExpB | VAR Char
    deriving Show

exemplo1 :: ExpB
exemplo1 = OR (AND(VAR 'x') (VAR 'y')) (NOT (VAR 'y'))

unique :: [Char] -> [Char]
unique [] = []
unique (x:xs) = x : unique [y | y <- xs , y /= x]

vars :: ExpB -> [Char]
vars e = unique (dale e)
    where 
        dale (VAR x) = [x]
        dale (OR x y) = dale x ++ dale y
        dale (AND x y) = dale x ++ dale y
        dale (NOT x) = dale x

eval :: [(Char, Bool)] -> ExpB -> Bool
eval t (VAR x) = snd (head (filter (\(nome, val) -> nome == x) t))
eval t (OR x y) = eval t x || eval t y
eval t (AND x y) = eval t x && eval t y
eval t (NOT x) = not (eval t x)