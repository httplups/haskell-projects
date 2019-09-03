-- reverte uma lista - FAZER p/ próxima aula - recursão com acum
-- reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x] 
-- nao pode fazer reverse' xs:[x] pq reverse' xs retorna uma lista e para usar 
-- : deve-se sempre ser element:array

reverte l = reverte' l []
            where
              reverte' [] acc = acc
              reverte' (x:xs) acc = reverte' xs (x:acc)


pot (x,y) = (x * x, y * y)


pares (a:as) = [ x | x <- (a:as), x `mod` 2 == 0]
sumall (a:as) = [a + (x+1) | x <- as]
-- x assume cada valor de as,sum mas o a e o as n mudam

contait it l =  contait' it l 0
            where contait' _ [] acc = acc
                  contait' it (x:xs) acc = 
                    if (it == x) 
                      then contait' it xs (acc+1) 
                      else contait' it xs acc


-- pos it l = pos' it l 1
--         where 
--           pos' it [] acc = acc
--           pos' it (x:xs) acc = 
--               if (it == x) 
--               then acc:(pos' it xs (acc+1))
--               else pos' it xs (acc+1)

qs [] = []
qs (x:xs) = qs menores ++ [x] ++ qs maiores
      where menores = [y | y <- xs, y <= x]
            maiores = [y | y <- xs, y >= x]

-- função recebe um array no qual o tipo é Ord, ja q vai ser comparação e vai retornar true ou false
ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (a:b:xs)
    | a <= b = ordenada (b:xs)
    | otherwise = False

-- ou ordenada (a:b:xs) = a<-b && ordenada (b:xs)
troca1 :: Eq a => a -> a -> [a] -> [a]
troca1 _ _ [] = []
troca1 n v (x:xs) 
  | x == v = [n] ++ (troca1 n v xs)
  | otherwise =  [x] ++ (troca1 n v xs)


data Ponto = Ponto Float Float -- x e y de um ponto

-- data ArvoreBinInt a = Nulo | No a (ArvoreBinInt a) (ArvoreBinInt a) deriving (Eq,Show,Read)
data Tree a = Nulo | No a (Tree a) (Tree a) deriving (Eq,Show,Read)
emOrdem :: Tree a -> [a]
emOrdem Nulo = []
emOrdem (No x esq dir) = (emOrdem esq) ++ [x] ++ (emOrdem dir)
-- arvEx = (No 2 (No 7 (No 2 Nulo Nulo)(No 6 (No 5 Nulo Nulo)(No 11 Nulo Nulo)))(No 5 Nulo(No 9 (No 4 Nulo Nulo)Nulo)))
-- (No 7 (No 3 (No 1 Nulo Nulo)(No 6 (No 4 Nulo Nulo)(No 6 Nulo Nulo)))(No 10 Nulo(No 14 (No 13 Nulo Nulo)Nulo)))

-- Achar elemento x numa ABB
findX :: Ord a => Tree a -> a -> Bool -- irá retornar um true or false
findX Nulo _ = False
findX (No el esq dir) x 
  | el == x = True
  | el < x = findX dir x
  | otherwise = findX esq x

findAbb :: Ord a => a -> Tree a -> Bool
findAbb _ Nulo = False
findAbb key (No a menor maior)
  | a == key = True
  | a < key = findAbb key maior
  | otherwise = findAbb key menor

  -- verifica se uma arvore é ABB
  -- isAbb :: Ord a => Tree a -> Bool  -- nao colocar a resticao de Ord na definicao do tipo!!
  -- isAbb Nulo = True
  -- isAbb (No _ Nulo Nulo) = True
  -- isAbb (No x Nulo ad) = isAbb ad && x < menor ad
  -- isAbb (No x ae Nulo) = isAbb ae && x > maior ae
  -- isAbb (No x ae ad) = isAbb ae && isAbb ad && x < menor ad && x > maior ae

  -- maior (No x _ Nulo) = x
  -- maior (No x _ ad) = maior ad

  -- menor (No x Nulo _) = x
  -- menor (No x ae _) = menor ae  

  -- applyTwice :: (a -> a) -> a -> a  
  -- applyTwice f x = f (f x)

  data Tree ch v = Nulo | No ch v (Tree ch v) (Tree ch v) deriving (Eq,Show,Read)

insert :: (Ord ch) => ch -> v -> Tree ch v -> Tree ch v
insert chave1 val1 Nulo = No chave1 val1 Nulo Nulo
  