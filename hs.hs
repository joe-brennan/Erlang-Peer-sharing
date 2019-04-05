-- HASKELL :O


maxThree :: Int -> Int -> Int -> Int
maxThree x y z = max z (max x y) 

funPair :: (t -> a) -> (t -> b) -> t -> (a, b)
funPair f g x = (f x, g x)


leng :: [a] -> Int
leng [] = 0
leng (h : t) = 1 + leng t

maxList :: [Int] -> Int
maxList [] = 0
maxList (h:t) = max h (maxList t)

g [] = []

g (x:xs) =
 if (x `mod` 2 == 0)
 then g xs
 else (2*x) : g xs
--  doubles all odd numbers and stores in a list

oddNo [] = []
oddNo (x:xs) f =
  map(f)