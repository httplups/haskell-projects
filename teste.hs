pos l it = pos' l it [] 1
where pos' [] _ acc _ = acc
      pos' (x:xs) it acc acc2 = if (x == a)
                                  then pos' xs a (acc++[acc2]) (acc2+1)
                                  else pos' xs it acc (acc2+1)