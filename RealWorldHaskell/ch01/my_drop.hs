-- version in the book :
-- myDrop n xs = if n <= 0 || null xs
--               then xs
--               else myDrop (n - 1) (tail xs)


-- My version :
myDrop :: (Num b, Eq b) => b -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop i (x:xs) = myDrop (i-1) xs
