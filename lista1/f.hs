type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa ops = f 0 ops
  where
    f acc [] = acc
    f acc (("Soma", v):xs) = f (acc + v) xs
    f acc (("Subtrai", v):xs) = f (acc - v) xs
    f acc (("Multiplica", v):xs) = f (acc * v) xs
    f _   (("Divide", 0):_) = -666
    f acc (("Divide", v):xs) = f (acc `div` v) xs

main = do
    a <- getLine
    let result = executa (read a)
    print result