--------------------------------------------------------------------------------
-- 🚀 HASKELL: O ARQUIVO DEFINITIVO PARA A PROVA
-- Resumo completo de Sintaxe, Funções Salva-Vidas e Lógica Estrutural.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PARTE 1: AS "FUNÇÕES DE OURO" (Se precisar ordenar ou tirar repetidos)
-- Decore ou leve anotado, pois não vêm no Prelude!
--------------------------------------------------------------------------------

-- 1. Tira duplicados mantendo a ordem
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique [y | y <- xs, y /= x]

-- 2. Ordena pelo primeiro elemento ou critério customizado (Quicksort)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn f (p:xs) = 
    sortOn f [y | y <- xs, f y >= f p]  -- Maior pro Menor (Mude para < se quiser Menor pro Maior)
    ++ [p] 
    ++ sortOn f [y | y <- xs, f y < f p]

--------------------------------------------------------------------------------
-- PARTE 2: GUIA DEFINITIVO DE SINTAXE (Os 3 Funcionários)
--------------------------------------------------------------------------------

-- 1. O DESEMPACOTADOR (Pattern Matching)
-- Útil para extrair dados. NÃO USA "=" DENTRO DOS PARÊNTESES, NÃO TESTA NADA.
somaTupla :: (Int, Int) -> Int
somaTupla (x, y) = x + y

-- 2. O TESTADOR (Guards / As barrinhas '|')
-- Útil para IF/ELSE. Sempre retorna True ou False. NÃO ESQUEÇA O 'otherwise'.
avaliaNota :: Float -> String
avaliaNota n
    | n >= 7.0  = "Passou"
    | n >= 4.0  = "Final"
    | otherwise = "Reprovou"

-- 3. A CAIXA DE FERRAMENTAS (O 'where')
-- Guarda variáveis para não repetir código. Fica no final, indentado com ESPAÇOS!
bhaskara :: Float -> Float -> Float -> (Float, Float)
bhaskara a b c = (raizPos, raizNeg)
  where
    delta   = (b * b) - (4 * a * c)
    raizPos = (-b + sqrt delta) / (2 * a)
    raizNeg = (-b - sqrt delta) / (2 * a)


--------------------------------------------------------------------------------
-- PARTE 3: O PADRÃO "PROVA" - MANIPULANDO DADOS COMPLEXOS
--------------------------------------------------------------------------------

-- O seu professor adora criar um 'data' e um 'type' e pedir para você 
-- cruzar os dados usando map/filter ou list comprehension.

data Time = Brasil | Escocia | Marrocos | Noruega deriving (Show, Eq)
type Jogo = (Time, Int, Int, Time)

jogosCopa :: [Jogo]
jogosCopa = [(Brasil, 2, 1, Escocia), (Marrocos, 2, 2, Noruega), (Brasil, 3, 0, Marrocos)]

-- Exemplo Clássico: Quantos gols um time fez no campeonato?
-- Lógica: Filtra quem jogou, mapeia os gols, soma tudo.
golsDoTime :: Time -> [Jogo] -> Int
golsDoTime t jogos = sum [ if t1 == t then g1 else g2 | (t1, g1, g2, t2) <- jogos, t1 == t || t2 == t ]

-- Exemplo Clássico 2: Quem são os classificados? (Juntando tudo)
classificados :: [Time] -> [Jogo] -> [Time]
classificados times jogos = 
    let tabela = map (\t -> (golsDoTime t jogos, t)) times -- 1. Gera [(Gols, Time)]
        tabelaOrdenada = sortOn fst tabela                 -- 2. Ordena pelos Gols (Maior pro Menor)
    in take 2 (map snd tabelaOrdenada)                     -- 3. Pega os 2 primeiros Times


--------------------------------------------------------------------------------
-- PARTE 4: AVALIADOR DE EXPRESSÕES (ÁRVORES ABSTRATAS)
--------------------------------------------------------------------------------

-- Questões de avaliar árvores booleanas ou matemáticas caem 99% das vezes.
-- O segredo é: Pattern Matching na esquerda, Recursão na direita.

data Exp = Num Int | Soma Exp Exp | Mult Exp Exp deriving Show

expExemplo :: Exp
expExemplo = Mult (Soma (Num 2) (Num 3)) (Num 4) -- Representa: (2 + 3) * 4

avaliaExp :: Exp -> Int
avaliaExp (Num x)      = x
avaliaExp (Soma e1 e2) = avaliaExp e1 + avaliaExp e2
avaliaExp (Mult e1 e2) = avaliaExp e1 * avaliaExp e2


--------------------------------------------------------------------------------
-- PARTE 5: ÁRVORES BINÁRIAS (TREES)
--------------------------------------------------------------------------------

-- A estrutura sagrada da Árvore Binária.
data Arvore a = Vazia | No a (Arvore a) (Arvore a) deriving Show

arv :: Arvore Int
arv = No 5 (No 3 (No 1 Vazia Vazia) Vazia) (No 8 Vazia Vazia)

-- 1. Contar quantos NÓS existem na árvore
contaNos :: Arvore a -> Int
contaNos Vazia = 0
contaNos (No _ esq dir) = 1 + contaNos esq + contaNos dir

-- 2. Mapear uma árvore (Aplicar uma função a todos os nós)
mapArvore :: (a -> b) -> Arvore a -> Arvore b
mapArvore _ Vazia = Vazia
mapArvore f (No v esq dir) = No (f v) (mapArvore f esq) (mapArvore f dir)

-- 3. Transformar Árvore em Lista (Esquerda -> Raiz -> Direita)
achataArvore :: Arvore a -> [a]
achataArvore Vazia = []
achataArvore (No v esq dir) = achataArvore esq ++ [v] ++ achataArvore dir


--------------------------------------------------------------------------------
-- 🔥 DICAS FINAIS DE SOBREVIVÊNCIA NA PROVA:
-- 1. Deu erro de "Show"? Faltou passar todos os argumentos pra função no terminal.
-- 2. Deu erro de "Type"? Você prometeu devolver `[Int]` mas devolveu `Int`.
-- 3. Use parênteses nas funções do Prelude: `sum (map f lista)` e nunca `sum map f lista`.
-- 4. Na dúvida entre Map/Filter e Compreensão de Lista, use Compreensão de 
--    Lista: `[x | x <- lista, condicao]`. O professor costuma dar ponto extra!
--------------------------------------------------------------------------------