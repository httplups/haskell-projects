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
  | x == v = n ++ (troca1 n v xs)
  | otherwise =  x ++ (troca1 n v xs)