lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs
