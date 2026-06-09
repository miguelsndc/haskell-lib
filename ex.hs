--------------------------------------------------------------------------------
-- MEGA LISTA DE EXERCÍCIOS HASKELL - PADRÃO DE PROVA
-- Assuntos: Listas, Tuplas, Tipos Algébricos (Data), Alta Ordem e Árvores.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- BLOCO 1: RECURSÃO E LISTAS SIMPLES
--------------------------------------------------------------------------------

-- EXERCÍCIO 1: Soma dos Pares
-- Crie uma função que receba uma lista de inteiros e retorne a soma de todos
-- os números que são pares.
-- Dica: 'even' é uma função nativa que retorna True se for par.
somaPares :: [Int] -> Int
somaPares [] = 0
somaPares (x:xs)
    | even x    = x + somaPares xs
    | otherwise = somaPares xs

-- EXERCÍCIO 2: Conta Vogais
-- Crie uma função que conte quantas vogais existem em uma string.
contaVogais :: String -> Int
contaVogais [] = 0
contaVogais (x:xs)
    | x `elem` "aeiouAEIOU" = 1 + contaVogais xs
    | otherwise             = contaVogais xs

-- EXERCÍCIO 3: Inserção Ordenada
-- Dado um número e uma lista já ordenada, insira o número na posição correta.
insereOrdenado :: Int -> [Int] -> [Int]
insereOrdenado n [] = [n]
insereOrdenado n (x:xs)
    | n <= x    = n : x : xs
    | otherwise = x : insereOrdenado n xs


--------------------------------------------------------------------------------
-- BLOCO 2: ALTA ORDEM E LIST COMPREHENSIONS (map, filter, etc)
--------------------------------------------------------------------------------

-- EXERCÍCIO 4: Tamanho das palavras que começam com 'A'
-- Dada uma lista de palavras, retorne uma lista com o tamanho das palavras 
-- que começam com a letra 'a' ou 'A'.
tamanhoPalavrasA :: [String] -> [Int]
tamanhoPalavrasA palavras = 
    [length p | p <- palavras, head p == 'A' || head p == 'a']

-- EXERCÍCIO 5: Produto Escalar (ZipWith)
-- O produto escalar de dois vetores [a,b] e [c,d] é (a*c) + (b*d).
-- Calcule usando funções nativas do Prelude.
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar xs ys = sum (zipWith (*) xs ys)

-- EXERCÍCIO 6: Separa Pares e Ímpares
-- Receba uma lista e devolva uma tupla com duas listas: (Pares, Impares)
separaParImpar :: [Int] -> ([Int], [Int])
separaParImpar xs = (filter even xs, filter odd xs)


--------------------------------------------------------------------------------
-- BLOCO 3: TIPOS DE DADOS CUSTOMIZADOS (DATA / TYPE)
--------------------------------------------------------------------------------

-- EXERCÍCIO 7: Lógica de Semáforo
-- Defina um tipo Semaforo e uma função que retorne a próxima cor.
data Semaforo = Verde | Amarelo | Vermelho 
    deriving (Show, Eq)

proximaCor :: Semaforo -> Semaforo
proximaCor Verde    = Amarelo
proximaCor Amarelo  = Vermelho
proximaCor Vermelho = Verde

-- EXERCÍCIO 8: Geometria e Casamento de Padrões
-- Defina um tipo Forma (Circulo raio | Retangulo base altura) e calcule a área.
data Forma = Circulo Float | Retangulo Float Float 
    deriving Show

area :: Forma -> Float
area (Circulo r) = 3.14 * r * r
area (Retangulo b h) = b * h

-- EXERCÍCIO 9: Sistema de Banco (Múltiplos construtores)
-- Receba uma lista de transações e calcule o saldo final.
data Transacao = Deposito Float | Saque Float 
    deriving Show

saldoFinal :: [Transacao] -> Float
saldoFinal [] = 0.0
saldoFinal (Deposito v : xs) = v + saldoFinal xs
saldoFinal (Saque v : xs)    = saldoFinal xs - v


--------------------------------------------------------------------------------
-- BLOCO 4: TUPLAS E ESTRUTURAS COMPLEXAS
--------------------------------------------------------------------------------

-- EXERCÍCIO 10: Sistema de Alunos
-- Um aluno é uma tupla (Nome, Nota1, Nota2). 
-- Retorne apenas os nomes dos alunos aprovados (Média >= 7.0).
type Aluno = (String, Float, Float)

aprovados :: [Aluno] -> [String]
aprovados alunos = [nome | (nome, n1, n2) <- alunos, (n1 + n2) / 2 >= 7.0]

-- EXERCÍCIO 11: O Mais Velho
-- Dada uma lista de (Nome, Idade), retorne o nome da pessoa mais velha.
-- Vamos usar o padrão de desempacotador clássico para treinar a lógica manual.
maisVelho :: [(String, Int)] -> String
maisVelho [] = error "Lista vazia"
maisVelho [(nome, _)] = nome -- Caso base: se só tem 1 pessoa, ela é a mais velha
maisVelho ((nome1, idade1) : (nome2, idade2) : xs)
    | idade1 >= idade2 = maisVelho ((nome1, idade1) : xs) -- Elimina o 2º
    | otherwise        = maisVelho ((nome2, idade2) : xs) -- Elimina o 1º


--------------------------------------------------------------------------------
-- BLOCO 5: ÁRVORES (TREES)
--------------------------------------------------------------------------------

-- Definindo uma Árvore Binária genérica
-- Uma árvore pode ser Vazia (Folha) ou ser um Nó contendo um valor e 2 sub-árvores.
data Arvore a = Folha | No a (Arvore a) (Arvore a) 
    deriving Show

-- Exemplo de árvore para testar no GHCi:
--        5
--       / \
--      3   8
--     / \   \
--    1   4   9
arvoreExemplo :: Arvore Int
arvoreExemplo = No 5 (No 3 (No 1 Folha Folha) (No 4 Folha Folha)) 
                     (No 8 Folha (No 9 Folha Folha))

-- EXERCÍCIO 12: Soma dos Nós da Árvore
-- Crie uma função que soma todos os números de uma Árvore Binária de Inteiros.
somaArvore :: Arvore Int -> Int
somaArvore Folha = 0
somaArvore (No valor esq dir) = valor + somaArvore esq + somaArvore dir

-- EXERCÍCIO 13: Busca em Árvore Binária
-- Crie uma função que verifica se um elemento existe dentro da árvore.
buscaArvore :: Eq a => a -> Arvore a -> Bool
buscaArvore _ Folha = False
buscaArvore alvo (No valor esq dir)
    | alvo == valor = True
    | otherwise     = buscaArvore alvo esq || buscaArvore alvo dir

-- EXERCÍCIO 14: Maior elemento de uma Árvore Binária de Busca (BST)
-- Em uma árvore de busca, o maior elemento SEMPRE está o mais à direita possível.
maiorDaArvore :: Arvore Int -> Int
maiorDaArvore Folha = error "Árvore vazia nao tem maximo"
maiorDaArvore (No valor _ Folha) = valor -- Se não tem filho à direita, ele é o maior
maiorDaArvore (No _ _ dir)       = maiorDaArvore dir -- Continua descendo pra direita

-- EXERCÍCIO 15: Transformar Árvore em Lista (In-Order)
-- Essa é clássica: Pega todos os elementos da árvore e coloca numa lista 
-- ordenando Esquerda -> Centro -> Direita.
arvoreParaLista :: Arvore a -> [a]
arvoreParaLista Folha = []
arvoreParaLista (No valor esq dir) = 
    arvoreParaLista esq ++ [valor] ++ arvoreParaLista dir