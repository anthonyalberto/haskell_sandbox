import System.IO
import System.Directory
import Data.List
import Data.Map as Map

-- main = do
--     handle <- openFile "todo.txt" ReadMode
--     (tempName, tempHandle) <- openTempFile "." "temp"
--     contents <- hGetContents handle
--     let todoTasks = lines contents
--         numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
--     putStrLn "These are your TO-DO items:"
--     putStr $ unlines numberedTasks
--     putStrLn "Which one do you want to delete?"
--     numberString <- getLine
--     let number = read numberString
--         newTodoItems = delete (todoTasks !! number) todoTasks
--     hPutStr tempHandle $ unlines newTodoItems
--     hClose handle
--     hClose tempHandle
--     removeFile "todo.txt"
--     renameFile tempName "todo.txt"

main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = Map.fromList $ zip [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr . unlines $ mapToStringListWithNumbers numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = Map.delete number numberedTasks
    hPutStr tempHandle . unlines $ Map.elems newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"


mapToStringListWithNumbers :: (Show a) => Map.Map a String -> [String]
mapToStringListWithNumbers = Map.foldrWithKey (\k v acc -> ((show k) ++ " - " ++ v) : acc) []

