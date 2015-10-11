main = interact palindrome

palindrome :: String -> String
palindrome xs = unlines $ map (palindromeSentence) allLines
    where allLines = lines xs
          palindromeSentence line = if line == reverse line
                                      then "palindrome"
                                      else "not palindrome"
