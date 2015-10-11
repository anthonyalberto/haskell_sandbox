import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
    where nlen = length needle

lsort :: [[a]] -> [[a]]
lsort = sortBy (compare `on` length)

-- all isAlphaNum "bobby283"
-- filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"

encode :: Int -> String -> String
encode n xs = map (chr . (+ n) . ord)  xs

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

lookUpValue :: (Eq k) => k -> [(k,v)] -> Maybe v
lookUpValue _ [] = Nothing
lookUpValue key ((k,v):xs)
    | key == k = Just v
    | otherwise = lookUpValue key xs

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,_) -> key == k) $ xs

lookUpValue' :: (Eq k) => k -> [(k,v)] -> Maybe v
lookUpValue' key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing


fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

phoneBook =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

phoneBookToMap' :: (Ord k) => [(k, String)] -> Map.Map k [String]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
