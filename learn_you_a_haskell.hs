len :: [a] -> Integer
len []=0
len (x:xs) = 1+ len xs

doubleSmallNumber x = if x>100 then x else x*2
