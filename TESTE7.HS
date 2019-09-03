data Tree ch v = Nulo | No ch v (Tree ch v) (Tree ch v) deriving (Eq,Show,Read)

insert :: (Ord ch) => ch -> v -> Tree ch v -> Tree ch v
insert chave1 val1 Nulo = No chave1 val1 Nulo Nulo
-- insert chave1 val1 (No ch v esq dir) 
--   | ch > chave1 = No ch val1 esq' dir
--   | otherwise = No ch val1 esq dir'
--   where
--   esq' = insert esq chave1 val1
--   dir' = insert dir chave1 val1