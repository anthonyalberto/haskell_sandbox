import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            , ("bump", bump)
            ]


main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] =
    editTodoFile fileName (\todoTasks ->
        let number = read numberString
        in  delete (todoTasks !! number) todoTasks
    )

bump :: [String] -> IO ()
bump [fileName, numberString] =
    editTodoFile fileName (\todoTasks ->
        let newFirstLine = todoTasks !! (read numberString)
        in  newFirstLine : delete newFirstLine todoTasks
    )

-- Takes a file name, and a function that, given the actual content, generates the content to be written
editTodoFile :: FilePath -> ([String] -> [String]) -> IO ()
editTodoFile fileName f = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle

    hPutStr tempHandle $ (unlines . f . lines) contents

    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
