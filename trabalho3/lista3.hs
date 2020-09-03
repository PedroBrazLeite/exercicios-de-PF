-- Ex 1.0 --
ou :: Bool -> Bool -> Bool
ou True True = True
ou False True = True
ou True False = True
ou False False = True

ou1 :: Bool -> Bool -> Bool
ou1 True _ = True
ou1 False False = False

ou2 :: Bool -> Bool -> Bool
ou2 False x = x
ou2 True _ = True

-- Ex 1.2 --
ou3 :: Bool -> Bool -> Bool
ou3 x y = if (x == False && y == False) then False else True

ou4 :: Bool -> Bool -> Bool
ou4 x y
  | x == True = True
  | y == True = True
  | otherwise = False

-- Ex 2.0 --
double :: Float -> Float
double x = x * 2

quadruple :: Float -> Float
quadruple x = (double (double x))

pit :: Float -> Float -> Float
pit o a = sqrt ((a ^ 2) + (o ^ 2))

dist :: Float -> Float -> Float -> Float -> Float
dist xi yi x y = sqrt (((x - xi) ^ 2) + ((y - yi) ^ 2))

-- Ex 3.0 PDF --

-- Ex 4.0 --
fat :: Int -> Int
fat x
  | x == 0 = 1
  | x == 1 = 1
  | otherwise = x * fat (x -1)

-- Ex 4.1 --
fat1 :: Int -> Int
fat1 0 = 1
fat1 1 = 1
fat1 x = x * fat (x -1)

-- 5.0 --
fibo :: Int -> Int
fibo n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibo (n -2) + fibo (n -1)

-- 6.0 --
n_tri :: Int -> Int
n_tri x
  | x == 0 = 0
  | x == 1 = 1
  | otherwise = x + n_tri (x - 1)

-- 7.0 --
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = (x, y + x)

fibo2 :: Int -> (Int, Int)
fibo2 x
  | x == 0 = (0, 1)
  | otherwise = passo (fibo2 (x -1))

-- 8.0 --
potencia2 :: Int -> Int
potencia2 x
  | x == 0 = 1
  | otherwise = 2 * potencia2 (x -1)

-- 9.0 --
prodIntervalo :: Int -> Int -> Int
prodIntervalo x y
  | x > y = 0
  | x == y = y
  | otherwise = y * prodIntervalo x (y -1)

-- 9.1 --
fatIntervalo :: Int -> Int
fatIntervalo x = prodIntervalo 1 x

-- 11.0 --
div_resto :: Int -> Int -> Int
div_resto x y
  | x < y = x
  | otherwise = div_resto (x - y) y

div_inteira :: Int -> Int -> Int
div_inteira x y
  | x < y = 0
  | otherwise = 1 + div_inteira (x - y) y

-- 12.0 --
mdcg :: (Int, Int) -> Int
mdcg (m, n)
  | n == 0 = m
  | otherwise = mdcg (n, (mod m n))

mdc :: (Int, Int) -> Int
mdc (m, 0) = m
mdc (m, n) = mdc (n, (mod m n))

-- 13.00 --
binog :: (Int, Int) -> Int
binog (n, k)
  | k == 0 = 1
  | k == n = 1
  | otherwise = binog (n -1, k) + binog (n -1, k -1)

binomial :: (Int, Int) -> Int
binomial (n, 0) = 1
binomial (n, k) =
  if (k == n)
    then 1
    else binomial (n -1, k) + binomial (n -1, k -1)

-- 14.0 PDF --

-- 15.0 --
type Lista = [Int]

enum :: Int -> Int -> Lista
enum a b
  | a == b = [a]
  | a > b = []
  | otherwise = [a, b]

-- 15.1 --
enumA :: Int -> Int -> Lista
enumA a b
  | a == b || a > b = []
  | otherwise = [(a+1)..(b-1)]
