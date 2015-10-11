import Data.Ratio
import Data.List (all)
import qualified Data.Map as Map

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs
instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []


flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

-- Example usages

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

regCoin = [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

foo :: Prob Bool
foo = coin >>= (\a ->
      coin >>= (\b ->
      loadedCoin >>= (\c ->
      return (all (==Tails) [a,b,c]))))

fooa = coin >>= (\a -> return a)


fooc = coin >>= (\a ->
      coin >>= (\b ->
      loadedCoin >>= (\c ->
      return c)))


flipTwo :: Prob [Coin]
flipTwo = do
    a <- coin
    c <- loadedCoin
    return [a,c]
    -- return (all (==Tails) [a,b,c])


-- Understanding what the fuck is happening
-- Doing

foofoo' = do
  a <- coin
  b <- loadedCoin
  return (all (==Tails) [a,b])

-- Is syntactic sugar for :

foofoo = coin >>= (\a ->
      loadedCoin >>= (\b ->
      return (all (==Tails) [a,b])))


-- * can be easier to understand by just returning [a,b] as the booleans will be replaced by [Heads, Heads], [Heads, Tails], [Tails, Heads], [Tails, Tails]

-- coin is = Prob [(Heads,1%2),(Tails,1%2)]
-- coin >>= (\a -> coin) will fmap the function onto each Heads or Tails value
-- that will become Prob [(Prob [(Heads,1%2),(Tails,1%2)],1%2),(Prob [(Heads,1%2),(Tails,1%2)],1%2)]
-- it then is flattened, that will basically multiply top probabilities into each inner one and remove the nesting
-- and that becomes : Prob [(Heads,1%4),(Tails,1%4),(Heads,1%4),(Tails,1%4)]
-- but here we're doing coin >>= (\a -> loadedCoin >>= (\b -> return (all (==Tails) [a,b])))
-- so we are calling (\a -> loadedCoin >>= (\b -> return (all (==Tails) [a,b])) with Heads and then Tails etc ... schema


--                   ------------------------coin------------------
-- a=          Heads/                                              \Tails
--           --loadedCoin--                              -----loadedCoin------                             coin >>= (\b -> return (all (==Tails) [a,b])
-- b=  Heads/              \Tails                  Heads/                     \Tails
--[a,b] [Heads, Heads]    [Heads, Tails]         [Tails, Heads]            [Tails, Tails]                  return (all (==Tails) [a,b]))) for each of those combos



--                   ----------------Prob [(Heads,1 % 2),(Tails,1 % 2)]----------------------
-- a=          Heads/                                                                        \Tails
--        Prob [(Heads,1 % 10),(Tails,9 % 10)]                            Prob [(Heads,1 % 10),(Tails,9 % 10)]                      coin >>= (\b -> return (all (==Tails) [a,b])
-- b=  Heads/                                \Tails                      Heads/                               \Tails
--[a,b] [Heads, Heads]                  [Heads, Tails]             [Tails, Heads]                         [Tails, Tails]
--  Prob(False, 1%1)                    Prob(False, 1%1)          Prob(False, 1%1)                        Prob(True, 1%1)     return (all (==Tails) [a,b]))) for each of those combos

-- When bubbling up, each Heads or Tails value gets replaced by the Prob() from the level below and we call flatten on each
-- So Prob [(Heads,1 % 10),(Tails,9 % 10)] from the left branch becomes :
-- Prob [(Prob(False, 1%1),1 % 10),(Prob(False, 1%1),9 % 10)]
-- then flattened = Prob [(False,1 % 10),(False,9 % 10)]

-- Prob [(Heads,1 % 10),(Tails,9 % 10)] from the right branch becomes :
-- Prob [(Prob(False, 1%1),1 % 10),(Prob(True, 1%1),9 % 10)]
-- then flattened = Prob [(False,1 % 10),(True,9 % 10)]


-- And finally Prob [(Heads,1 % 2),(Tails,1 % 2)] becomes :
-- Prob [(Prob [(False,1 % 10),(False,9 % 10)],1 % 2),(Prob [(False,1 % 10),(True,9 % 10)],1 % 2)]
-- then flattened = Prob [(False,1 % 20),(False,9 % 20),(False,1 % 20),(True,9 % 20)]


-- So we can rewrite the tree above with the final values (keep in mind probabilites are multiplied when we flatten the nested Probs):
-- probably easier to read from the bottom-up

--                 -------------------Prob [(False,1 % 20),(False,9 % 20),(False,1 % 20),(True,9 % 20)]--------------------
--                /                                                                                                        \
-- Prob [(False,1 % 10),(False,9 % 10)]                                                                     Prob [(False,1 % 10),(True,9 % 10)]
--    /                          \                                                                           /                             \
-- Prob(False, 1%1)         Prob(False, 1%1)                                                        Prob(False, 1%1)                 Prob(True, 1%1)


-- Now for the extra function to group the same results :
-- Map.fromListWith will build a Map from the list. When there's a duplicate value, it calls (+) with both probabilities so we can add them
-- We then have to cast the map back to a List so we can feed it to Prob again

groupSimilarProbs :: Ord a => Prob a -> Prob a
groupSimilarProbs (Prob xs) = Prob . Map.toList $ Map.fromListWith (+) xs
