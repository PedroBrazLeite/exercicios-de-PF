-- Ex 2 --
quadrado :: Int -> Int -> [Int]
quadrado a b = [x ^ 2 | x <- [a .. b]]

-- Ex 3 --
seleciona_impares :: [Int] -> [Int]
seleciona_impares lista = [x | x <- lista, odd x]

-- Ex 4 --
tabuada :: Int -> [Int]
tabuada num = [num * x | x <- [1 .. 10]]

-- Ex 5 --
bissexto :: Int -> Bool
bissexto x
  | mod x 4 /= 0 = False
  | mod x 100 /= 0 = True
  | mod x 400 /= 0 = False
  | otherwise = True

bissextos :: [Int] -> [Int]
bissextos lista = [x | x <- lista, bissexto x]

-- Ex 6 --
sublistas :: [[a]] -> [a]
sublistas lista = [x | y <- lista, x <- y]

-- ex 7 --
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
    ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
    ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

-- ex 8 --
npares :: [Int]  -> [Int]
npares par = [x |x <- par ,even x]

-- Ex 9 --
prod :: [Int] -> Int
prod [] = 0
prod [x] = x
prod (head:tail) = head * prod tail

-- ex 10 --
comprime::[[a]] -> [a]
comprime [[]] = []
comprime ([]:tail) = comprime tail
comprime ((head:head1):tail) = head:(comprime(head1:tail))

-- Ex 11 --
tamanho:: [a] -> Int
tamanho [] = 0
tamanho (head : tail) = 1 + tamanho tail

-- Ex 12 --
uniaoNRec ::Eq a => [a] -> [a] -> [a] 
uniaoNRec list1 list2 = [x | x<-list1] ++ [y | y<-list2, elem y list1 == False]

-- Ex 13 --
uniaoRec2 :: [Int] -> [Int] -> [Int]
uniaoRec2 (a : as) (x : xs)
  | (a == x) = uniaoRec2 as xs
  | otherwise = a : as ++ x : xs