import Control.Monad.State

type Stack = [Int]

-- pop :: Stack -> (Int,Stack)
-- pop (x:xs) = (x,xs)

-- push :: Int -> Stack -> ((),Stack)
-- push a xs = ((),a:xs)

-- stackManip :: Stack -> (Int, Stack)
-- stackManip stack = let
--     ((),newStack1) = push 3 stack
--     (a ,newStack2) = pop newStack1
--     in pop newStack2


-- pop :: State Stack Int
-- pop = State $ \(x:xs) -> (x,xs)

-- push :: Int -> State Stack ()
-- push a = State $ \xs -> ((),a:xs)


-- stackManip :: State Stack Int
-- stackManip = do
--     push 3
--     a <- pop
--     pop


-- stackStuff :: State Stack ()
-- stackStuff = do
--     a <- pop
--     if a == 5
--         then push 5
--         else do
--             push 3
--             push 8


-- FROM http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/
-- fromStoAandS :: Int -> (String,Int)
-- fromStoAandS c | c `mod` 5 == 0 = ("foo",c+1)
--                | otherwise = ("bar",c+1)

-- stateIntString :: State Int String
-- stateIntString = State fromStoAandS

-- push/pop, stack simulation without using State
pop :: [Int] -> (Int, [Int])
pop (x:xs) = (x, xs)

push :: Int -> [Int] -> ((), [Int])
push x xs = ((), x:xs)

simulateStack s = let
    (_, s1) = push 3 s
    (x, s2) = pop s1
    (_, s3) = push (x * x) s2
    in pop s3

res = simulateStack [1, 2, 3]


-- push/pop, stack simulation using State and bind
pop' :: State [Int] Int
pop' = state (\(x:xs) -> (x, xs))

push':: Int -> State [Int] ()
push' x = state (\xs -> ((), x:xs))

-- This can explain what happens during composition below
-- (>>=) :: State s a -> (a -> State s b) -> State s b
-- processor >>= processorGenerator = State $ \ st ->
--                                    let (x, st') = runState processor st
--                                    in runState (processorGenerator x) st'

simulateStack' = (push' 3) >>= (\_ -> pop') >>= (\x -> push' (x * x)) >>= (\x -> pop')

res' = runState simulateStack' [1, 2, 3]

-- stack simulation using State and do notation

simulateStack'' = do
    push' 3
    x <- pop'
    push' (x * x)
    pop'

res'' = runState simulateStack'' [1, 2, 3]



stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]
