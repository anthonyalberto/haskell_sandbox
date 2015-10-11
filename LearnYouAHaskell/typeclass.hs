import Tree

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

class YesNo a where
    yesno :: a -> Bool
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
instance YesNo Bool where
    yesno = id
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult


class Tofu t where
    tofu :: j a -> t a j
data Frank a b  = Frank {frankField :: b a} deriving (Show)
instance Tofu Frank where
    tofu x = Frank x


data Barry t k p = Barry { yabba :: p, dabba :: t k } deriving (Show)
instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
