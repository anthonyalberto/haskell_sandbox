type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise                    = Left $ formatErrorMessage "left" (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Right (left, right + n)
    | otherwise                    = Left $ formatErrorMessage "right" (left, right + n)

banana :: Pole -> Either String Pole
banana pole = Left (formatErrorMessage "banana" pole)

formatErrorMessage :: String -> Pole -> String
formatErrorMessage fell (left,right) = "Fell on the " ++ fell ++ ". Left : " ++ show left ++ ". Right : " ++ show right


routine :: Either String Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

routine' :: Either String Pole
routine' = do
    start <- return (0,0)
    first <- banana start
    second <- landLeft 2 first
    third <- landRight 2 second
    fourth <- landRight 5 third
    landRight 3 fourth


-- Better : just use >>= in this context
-- landLeft 1 (0,0) >>= landRight 2 >>= landLeft 3 >>= landLeft 1 >>= banana
