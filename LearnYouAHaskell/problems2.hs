import Data.List
lsort :: [[a]] -> [[a]]
lsort = sortBy (\xs ys -> compare (length xs) (length ys))

lfsort :: [[a]] -> [[a]]
lfsort xs = (concat . lsort . groupBy equalLength . lsort) xs
    where equalLength ys zs = length ys == length zs

isPrime :: (Integral a, Enum a) => a -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = foldl (accTrueWhileNotDivisable) True [2..(n-1)]
    where accTrueWhileNotDivisable acc x = if n `mod` x == 0 then False else acc && True

coprime :: (Integral a) => a -> a -> Bool
coprime m n = gcd m n == 1

primesR :: (Integral a) => a -> a -> [a]
primesR m n = [x | x <- [m..n], isPrime x]
