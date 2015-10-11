module Tree
( Tree(..)
, singleton
, treeInsert
, treeElem
, treeFromList
) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem value (Node a left right)
    | value == a = True
    | value < a = treeElem value left
    | otherwise = treeElem value right

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldr treeInsert EmptyTree


