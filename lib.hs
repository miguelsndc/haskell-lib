Aqui está o documento completo! Esse é o seu arsenal definitivo para qualquer prova ou projeto em Haskell.

Na primeira parte, deixei as 3 funções essenciais que **não** vêm no Prelude (para você copiar e colar). Logo em seguida, listei todas as funções úteis que **já vêm no Prelude** (prontas para uso, sem importar nada), com explicações diretas e exemplos de como aplicar.

```haskell
--------------------------------------------------------------------------------
-- GUIA DEFINITIVO: FUNÇÕES HASKELL PARA PROVAS E PROJETOS
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PARTE 1: AS 3 FUNÇÕES DE OURO QUE NÃO ESTÃO NO PRELUDE
-- (Copie e cole no seu código quando precisar)
--------------------------------------------------------------------------------

-- 1. unique (ou nub): Remove elementos duplicados mantendo a ordem original.
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique [y | y <- xs, y /= x]

-- 2. sort: Ordena uma lista do menor para o maior (Quicksort simples).
sort :: Ord a => [a] -> [a]
sort [] = []
sort (pivo:xs) = 
    sort [y | y <- xs, y < pivo] 
    ++ [pivo] 
    ++ sort [y | y <- xs, y >= pivo]

-- 3. sortOn: Ordena uma lista baseada no resultado de uma função extraída do elemento.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn f (pivo:xs) = 
    sortOn f [y | y <- xs, f y < f pivo] 
    ++ [pivo] 
    ++ sortOn f [y | y <- xs, f y >= f pivo]


--------------------------------------------------------------------------------
-- PARTE 2: O PRELUDE COMPLETO (Já vem pronto para uso, sem importar nada)
--------------------------------------------------------------------------------

-- ==========================================
-- A. FUNÇÕES DE ALTA ORDEM (AS MAIS USADAS)
-- ==========================================

-- map :: (a -> b) -> [a] -> [b]
-- Para que serve: Aplica uma mesma função a todos os itens de uma lista.
-- Como usar: map (*2) [1, 2, 3]  ---> Retorna: [2, 4, 6]

-- filter :: (a -> Bool) -> [a] -> [a]
-- Para que serve: Filtra a lista, mantendo apenas os itens que passam na condição.
-- Como usar: filter (>2) [1, 2, 3, 4]  ---> Retorna: [3, 4]

-- foldl / foldr :: (b -> a -> b) -> b -> [a] -> b
-- Para que serve: Acumula/Reduz uma lista inteira a um único valor usando uma função.
-- Como usar: foldl (+) 0 [1, 2, 3]  ---> Retorna: 6


-- ==========================================
-- B. CORTAR E NAVEGAR EM LISTAS
-- ==========================================

-- take :: Int -> [a] -> [a]
-- Para que serve: Pega apenas os N primeiros elementos da lista.
-- Como usar: take 2 ["a", "b", "c"]  ---> Retorna: ["a", "b"]

-- drop :: Int -> [a] -> [a]
-- Para que serve: Remove os N primeiros elementos e retorna o que sobrou.
-- Como usar: drop 2 ["a", "b", "c"]  ---> Retorna: ["c"]

-- head :: [a] -> a
-- Para que serve: Pega O PRIMEIRO elemento da lista. (Dá erro se for vazia).
-- Como usar: head [10, 20, 30]  ---> Retorna: 10

-- tail :: [a] -> [a]
-- Para que serve: Retorna a lista inteira, EXCETO o primeiro elemento.
-- Como usar: tail [10, 20, 30]  ---> Retorna: [20, 30]

-- last :: [a] -> a
-- Para que serve: Pega O ÚLTIMO elemento da lista.
-- Como usar: last [10, 20, 30]  ---> Retorna: 30

-- init :: [a] -> [a]
-- Para que serve: Retorna a lista inteira, EXCETO o último elemento.
-- Como usar: init [10, 20, 30]  ---> Retorna: [10, 20]

-- (!!) :: [a] -> Int -> a
-- Para que serve: Pega o elemento que está em um índice específico (o índice começa no 0).
-- Como usar: ["zero", "um", "dois"] !! 1  ---> Retorna: "um"


-- ==========================================
-- C. OPERAÇÕES BÁSICAS COM LISTAS
-- ==========================================

-- length :: [a] -> Int
-- Para que serve: Conta quantos elementos existem na lista.
-- Como usar: length [1, 2, 3]  ---> Retorna: 3

-- (++) :: [a] -> [a] -> [a]
-- Para que serve: Junta (concatena) duas listas em uma só.
-- Como usar: [1, 2] ++ [3, 4]  ---> Retorna: [1, 2, 3, 4]

-- reverse :: [a] -> [a]
-- Para que serve: Inverte a ordem de todos os elementos.
-- Como usar: reverse [1, 2, 3]  ---> Retorna: [3, 2, 1]

-- elem :: Eq a => a -> [a] -> Bool
-- Para que serve: Verifica se um item existe dentro da lista (True ou False).
-- Como usar: elem 2 [1, 2, 3]  ---> Retorna: True

-- null :: [a] -> Bool
-- Para que serve: Verifica se a lista está vazia.
-- Como usar: null []  ---> Retorna: True


-- ==========================================
-- D. TUPLAS E PARES
-- ==========================================

-- fst :: (a, b) -> a
-- Para que serve: Pega o 1º elemento de uma tupla (só funciona com tuplas de 2).
-- Como usar: fst (1, "A")  ---> Retorna: 1

-- snd :: (a, b) -> b
-- Para que serve: Pega o 2º elemento de uma tupla (só funciona com tuplas de 2).
-- Como usar: snd (1, "A")  ---> Retorna: "A"

-- zip :: [a] -> [b] -> [(a, b)]
-- Para que serve: Costura duas listas em uma única lista de pares.
-- Como usar: zip [1, 2] ["a", "b"]  ---> Retorna: [(1, "a"), (2, "b")]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- Para que serve: Busca uma chave em uma lista de tuplas e retorna o valor (envolto em Just).
-- Como usar: lookup 'x' [('x', 10), ('y', 20)]  ---> Retorna: Just 10


-- ==========================================
-- E. MATEMÁTICA E LÓGICA EM LISTAS
-- ==========================================

-- sum :: Num a => [a] -> a
-- Para que serve: Soma todos os números de uma lista.
-- Como usar: sum [10, 20, 30]  ---> Retorna: 60

-- product :: Num a => [a] -> a
-- Para que serve: Multiplica todos os números de uma lista.
-- Como usar: product [2, 3, 4]  ---> Retorna: 24

-- maximum / minimum :: Ord a => [a] -> a
-- Para que serve: Acha o MAIOR ou o MENOR elemento de uma lista.
-- Como usar: maximum [1, 9, 2]  ---> Retorna: 9

-- any / all :: (a -> Bool) -> [a] -> Bool
-- Para que serve: 
--   any: Retorna True se PELO MENOS UM elemento passa no teste.
--   all: Retorna True se TODOS os elementos passam no teste.
-- Como usar: any even [1, 3, 5]  ---> Retorna: False


-- ==========================================
-- F. OPERADORES ÚTEIS DE SINTAXE
-- ==========================================

-- ($)
-- Para que serve: Remove a necessidade de abrir e fechar parênteses. Tudo à direita do $ roda primeiro.
-- Como usar: sum $ map (*2) [1..5]  ---> Faz a mesma coisa que: sum (map (*2) [1..5])

-- (.)
-- Para que serve: Composição de funções. Junta duas funções em uma só (da direita para a esquerda).
-- Como usar: (sum . map (*2)) [1, 2, 3]  ---> Retorna: 12

-- flip
-- Para que serve: Inverte a ordem que uma função recebe seus dois primeiros argumentos.
-- Como usar: flip (-) 10 30  ---> Ao invés de (10 - 30), ele faz (30 - 10) e retorna 20.

```