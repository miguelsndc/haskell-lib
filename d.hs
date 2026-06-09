situacao :: Double -> String
situacao n
  | n >= 7.0 = "Aprovado"
  | n >= 5.0 && n < 7.0 = "Recuperacao"
  | otherwise = "Reprovado"
media :: [Double] -> Double
media [] = 0.0
media n = sum n / fromIntegral(length n)

processaAlunos :: [(String, [Double])] -> [(String, Double, String)]
processaAlunos = map(\(nome,notas) -> let m = media notas in (nome, m, situacao m))

main = do
  s <- getLine
  let entrada = read s :: [(String, [Double])]
  let result = processaAlunos entrada
  print result