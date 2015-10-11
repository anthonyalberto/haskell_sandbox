-- Find the last element of a list
myLast :: [a] -> a
myLast = foldl1 (\_ x -> x)

myButLast :: [a] -> a
myButLast x = reverse x !! 1


--  Find the K'th element of a list. The first element in the list is number 1.
elementAt :: (Num b, Ord b) => [a] -> b -> a
elementAt [] _ = error "Empty list"
elementAt (x:xs) n
    | n <= 0 = error "Negative or null index"
    | n == 1 = x
    | otherwise = elementAt xs (n-1)

myLength :: (Num b) => [a] -> b
myLength = foldl (\acc _ -> acc + 1) 0

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- remove repeated values from a list
compress :: (Eq a) => [a] -> [a]
compress xs = foldr (\x acc -> if x == head acc then acc else x:acc) [last xs] xs

-- Regroup consecutive same elements in separate sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

-- Encode pack on tuples like (number, element)
encode :: (Eq a) => [a] -> [(Int, a)]
-- encode xs = foldr (\x acc -> ((length x), (head x)):acc) [] (pack xs)
-- OR
encode xs = [(length x, head x) | x <- pack xs]

-- duplicate every element of a list
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

-- replicate every element a given number of times
repli :: [a] -> Int -> [a]
repli xs n = foldl (\acc x -> acc ++ repliElem x n) [] xs
    where
      repliElem _ 0 = []
      repliElem x n = x : repliElem x (n-1)

-- Drop every Nth element of a list
-- Closure, we keep access to the original n variable throughout the unfolding of helper recursive execution
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n
    where helper [] _ = []
          helper (x:xs) 1 = helper xs n
          helper (x:xs) k = x : helper xs (k-1)

-- Split a list into two parts; the length of the first part is given.
-- split :: [a] -> Int -> ([a], [a])
-- split [] _ = ([], [])
-- split (x:xs) n
--     | n > 1 = ((x : (fst . split xs) (n-1)), (snd . split xs) (n-1))
--     | n == 1 = ([x], xs)


split :: [a] -> Int -> ([a], [a])
split (x:xs) n
    | n > 1 = let (first,second) = split xs (n-1)
              in (x : first, second)
    | otherwise = ([x], xs)


slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) n1 n2
  | n2 < 1 || n2 < n1 = []
  | n1 <= 1 = x : sliceRest
  | otherwise = sliceRest
  where sliceRest = slice xs (n1-1) (n2-1)

rotate :: [a] -> Int -> [a]
rotate xs n = drop len xs ++ take len xs
    where len = n `mod` length xs


removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Empty list or index out of bound"
removeAt 0 (x:xs) = (x, xs)
removeAt i (x:xs) = (first, x : second)
    where (first,second) = removeAt (i-1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 y xs = y : xs
insertAt _ _ [] = error "Index out of bound"
insertAt i y (x:xs) = x : insertAt (i-1) y xs

range :: (Num a, Ord a) => a -> a -> [a]
range i j
    | i > j = []
    | otherwise = i : range (i+1) j


combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1], x <- combinations (n-1) (drop (i+1) xs) ]

