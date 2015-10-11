sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs


maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
-- reverse' = foldl (\acc x -> x : acc) []
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- map ($ 3) [(4+), (10*), (^2), sqrt]
-- let maps = map (*) [1, 2, 3, 4, 5]
-- map ($ 3) maps


-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
-- map (negate . sum . tail) [[1..5],[3..6],[1..7]]

-- sum (replicate 5 (max 6.7 8.9))
-- (sum . replicate 5 . max 6.7) 8.9

-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]

-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- fn = celing . negate . tan . cos . max 50

-- oddSquareSum :: Integer
-- oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit
