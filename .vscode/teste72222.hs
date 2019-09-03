import Data.Char
data Tree ch v = Nulo | No ch v (Tree ch v) (Tree ch v) deriving (Eq,Show,Read)

insert :: (Ord ch) => ch -> v -> Tree ch v -> Tree ch v
insert key x Nulo = No key x Nulo Nulo
insert key x (No ch v esq dir)
  | ch > key = No ch v esq' dir
  | ch == key = No ch x esq dir
  | otherwise = No ch v esq dir'
  where
    esq' = insert key x esq
    dir' = insert key x dir

posicoes:: (Eq a, Num b) => a -> [a] -> [b]
posicoes x (a:as) = pos' x (a:as) 0 []
  where pos' _ [] acc ret = ret
        pos' x (a:as) acc res
          | x == a = pos' x as (acc+1) (res ++ [acc])
          | otherwise = pos' x as (acc+1) res

maiuscula = (`elem` ['A'..'Z'])

duplica x = 2*x

aplica2 f x = f (f x)
aplica2' f x = f $ f x

zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (a:as) (b:bs) = f a b : (zipWith' f as bs)

-- f = x ^ y por exemplo, logo g = y ^ x
flip' f = g  
    where g x y = f y x  

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (a:as) = f a : (map' f as)

filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)   
    | (p x) == True       = x : filter' p xs  
    | otherwise = filter' p xs 

-- sum' lista recebe a função foldl - começa a dobrar a partir do inicio
-- e recebe uma função anonima na qual recebe 2 params: acc e x -> inicio da lista
-- retorna um novo acumulador (acc + x) e a passagem de valores inicial é feita por 0 e xs
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs

  -- foldl retorna um valor acumulador, começa do início da lista

map'' :: (a -> b) -> [a] -> [b]  
-- a função map' pega uma função e uma lista e mapeia a função a cada elemento da lista
-- foldr recebe primeiro o ultimo elemento e dps o acumulador
-- note que ao final ([] xs) não segue a ordem de x acc, é invertido mesmo
map'' f xs = foldr (\x acc -> f x : acc) [] xs

maximo xs = foldl(\acc x -> if(x > acc) then x else acc) 0 xs

reverter xs = foldr(\x acc -> acc ++ [x]) [] xs

-- assume que o acc é o primeiro elemento da lista
sum'' xs = foldl1 (+) xs -- fazendo curry fica sum = foldl1 (+)
impar x = x `mod` 2 == 1
par x = x `mod` 2 == 0

filter'' f xs = foldr(\x acc -> if(f x) then x:acc else acc) [] xs

tamanho xs = foldl(\acc x -> acc+1) 0 xs

somacond xs = foldr(\x acc -> if (par x == True) then acc+x else acc) 0 xs

last' :: [a] -> a
last' = foldl1(\_ x -> x)  

existe it xs = foldl(\acc x -> if(x == it) then True else acc) False xs

conta it xs = foldl(\acc x -> if(x == it) then (acc+1) else acc) 0  xs

posicoes'' it l = fst (foldl (\(acc,n) x -> 
  if x==it then (acc++[n], n+1) else (acc,n+1))([],0) l) 

sem_ult xs = foldr(\x (acc, n) -> if(n == 0) then (acc, n+1) else (x:acc, n+1)) ([],0) xs

soma1 ch [] = [(ch,1)]
soma1 ch ((a,b):xs) 
    | ch == a = (a,b+1):xs
    | otherwise = (a,b):soma1 ch xs



isvogal = (`elem` ['a','e','i','o','u'])

-- vogal' xs = filter isvogal $ map toLower xs
vogal xs = foldr (\x acc -> soma1 x acc) [] (filter isvogal $ map toLower xs)
maior xs = maximum $ map snd $ xs
comum vogal = fst $ foldl1 (\acc x -> if (snd x) == (maior vogal) then x else acc) vogal

vogalmais l = let
  v1 = filter (`elem` "aeiou") $ map toLower l
  v2 = foldl (flip soma1) [] v1
  v3 = snd $ maximum [(b,a) | (a,b) <- v2]
  in v3

-- vogalmais2 xs = snd $ maximum $ map troca $ foldl (flip soma1) [] $ filter (`elem` "aeiou") $ map toLower xs
--   where troca = [(b,a) | (a,b) <- troca]