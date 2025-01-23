{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Data.List
import System.Directory
import System.IO
import System.Process
import System.Random (randomRIO)

enumerateTasks = zipWith (\number line -> show number ++ " - " ++ line) [0 ..]

todoFile = "todo.txt"

pressEnterToContinue = do
  putStrLn "Press <Enter> to continue"
  getLine

readTodoTasks filePath = do
  contents <- readFile filePath
  let todoTasks = lines contents
  return todoTasks

addNewTodoItem = do
  putStr "Write a new TO-DO thing > "
  hFlush stdout
  newTodoItem <- getLine
  appendFile todoFile (newTodoItem ++ "\n")
  putStrLn "New TO-DO item added!"

deleteTodoItem = do
  todoTasks <- readTodoTasks todoFile
  let numberedTodoTasks = enumerateTasks todoTasks
  putStrLn "These are your TO-DO items:"
  mapM_ putStrLn numberedTodoTasks
  putStr "Which one do you want to delete? > "
  hFlush stdout
  -- {-# WARNING It will explode if gets a wrong number #-}
  numberStr <- getLine
  let number = read numberStr
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle newTodoItems
  hClose tempHandle
  removeFile todoFile
  renameFile tempName todoFile

viewTodoItems = do
  todoTasks <- readTodoTasks todoFile
  putStrLn "These are your TO-DO items:"
  mapM_ (putStrLn . (" · " ++)) todoTasks

main = do
  _ <- system "clear"
  putStrLn "Welcome to the shittiest TO-DO List app, made by Lima 2024"
  putStrLn "Select what you want to do:"
  putStrLn " 1 - Add a new TO-DO item"
  putStrLn " 2 - Delete a TO-DO item"
  putStrLn " 3 - View the TO-DO list"
  putStrLn " 4 - Blow up Sauron's army"
  putStrLn " 5 - Exit"
  putStr "Enter some option > "
  hFlush stdout
  optionStr <- getLine
  let option = read optionStr
  case option of
    1 -> addNewTodoItem >> pressEnterToContinue >> main
    2 -> deleteTodoItem >> pressEnterToContinue >> main
    3 -> viewTodoItems >> pressEnterToContinue >> main
    4 -> do
      num <- randomRIO (1, 1500000) :: IO Int
      putStrLn $ "Kaboom! " ++ show num ++ " Ogres were dead or severely injured."
      pressEnterToContinue
      main
    5 -> putStrLn "Bye"
    _ -> putStrLn "Invalid Option! Try Again" >> pressEnterToContinue >> main
