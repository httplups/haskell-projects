data Tree a = Nulo | No a (Tree a) (Tree a) deriving (Eq,Show,Read)

isAbb :: Ord a => Tree a -> Bool  -- nao colocar a resticao de Ord na definicao do tipo!!
isAbb Nulo  = True
isAbb (No _ Nulo Nulo)  = True --toda folha é abb
isAbb (No x Nulo ad) = isAbb ad && x < menor ad
isAbb (No x ae Nulo) = isAbb ae && x > maior ae
isAbb (No x ae ad) = isAbb ae && isAbb ad && x < menor ad && x > maior ae

data Tree1 ch v = Vazia | Node ch v (Tree1 ch v) (Tree1 ch v) deriving (Eq,Show,Read)
insereAbb :: Ord ch => ch -> v -> Tree1 ch v -> Tree1 ch v 

maior :: Tree a -> a
maior (No x _ Nulo) = x
maior (No x _ ad) = maior ad

menor :: Tree a -> a
menor (No x Nulo _) = x
menor (No x ae _) = menor ae

insert :: Ord a => Tree a -> a -> Tree a
insert Nulo val = No val Nulo Nulo
insert (No x esq dir) val
  | x > val = No x esq' dir
  | otherwise = No x esq dir'
  where
  esq' = insert esq val
  dir' = insert dir val

delete :: Ord a => Tree a -> a -> Tree a
delete Nulo val = Nulo
delete (No x esq dir) val
  | x > val = No x esq' dir
  | x < val = No x esq dir'
  | esq == Nulo = dir
  | dir == Nulo = esq
  | otherwise = No min esq (delete dir min) -- procura o menor elemento da dir e remove ele da direita
  where
    esq' = delete esq val
    dir' = delete dir val
    min = menor dir

converteparaabb :: Ord a => [a] -> Tree a
converteparaabb lista  = converte' lista Nulo
    where converte' [] acc = acc
          converte' (x:xs) acc = converte' xs newacc
                    where newacc = insert acc x

split l e = split' l e []
split' [] _ acc = [acc,[]]
split' (x:xs) e acc 
    | x == e = [acc,xs]
    | otherwise = split' xs e (acc++[x])


