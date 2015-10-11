import Data.List.Split
import Data.Char

-- My first version, pretty terrible
-- solveRPN :: String -> Float
-- solveRPN = head . foldl processStack [] . words
--     where partsToProcess stack = fst $ splitAt 2 stack
--           partsToKeep stack = snd $ splitAt 2 stack
--           leftOperand stack = last $ partsToProcess stack
--           rightOperand stack = head $ partsToProcess stack
--           operator o = case o of "*" -> (*)
--                                  "+" -> (+)
--                                  "-" -> (-)
--                                  "/" -> (/)
--           calculateStack stack o = (operator o) (leftOperand stack) (rightOperand stack) : partsToKeep stack
--           areDigits = all isDigit
--           processStack acc x = if areDigits x
--                                    then (read x :: Float) : acc
--                                    else calculateStack acc x


-- correct implementation
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ** x):ys
            foldingFunction (x:xs) "ln" = log x:xs
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString:xs
