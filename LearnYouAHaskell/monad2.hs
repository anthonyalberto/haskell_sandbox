import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Instances
-- applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
-- applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])
-- applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

-- ("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))

-- applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
-- applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)
-- newtype Writer w a = Writer { runWriter :: (a,w) }

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)


newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)


