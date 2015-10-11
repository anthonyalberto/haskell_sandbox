-- Remember that when you're making functions, especially higher order ones, and you're unsure of the type,
-- you can just try omitting the type declaration and then checking what Haskell infers it to be by using :t.

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

fiveDividedBy :: (Floating a) => a -> a
fiveDividedBy = (5/)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

subtract4 :: (Num a) => a -> a
subtract4 = subtract 4


applyTwice :: (a -> a) -> (a -> a)
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--     where g x y = f y x

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- map' (+1) [1..100]
-- OR
-- [x + 1 | x <- [1..100]]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerSorted = quicksort' (filter (<=x) xs)
        biggerSorted = quicksort' (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted


largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- sum(takeWhile (<10000) [y | y <- [x^2 | x <- [1..]], odd y])
-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

collatzChain :: (Integral a) => a -> [a]
collatzChain n
    | n <= 0 = []
    | n == 1 = [1]
    | even n = n : collatzChain (n `div` 2)
    | otherwise = n : collatzChain (n*3 + 1)


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)


-- length (filter (>15) (map (length) (map (chain) [1..100])))
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- lambdas
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))


flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

