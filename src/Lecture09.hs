{-# LANGUAGE DuplicateRecordFields #-}

module Lecture09 where
import System.Directory
import System.Random
import Data.UUID
import Data.List
{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show)

newtype Title = Title String deriving (Eq, Show)

newtype Deadline = Deadline String deriving (Eq, Show)

instance Ord Deadline where
  compare (Deadline first) (Deadline second) = compare first second

newtype Content = Content String deriving (Eq, Show)


-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show)

instance Ord Todo where
  compare Todo{deadline=first} Todo{deadline=second} = compare first second

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  createDirectory rootFolder
  return $ TodoList rootFolder

getRandomId :: IO String
getRandomId = do
  g <- newStdGen
  let (uuid, _) = random g
  return $ toString uuid

getTodoFromString :: String -> Todo
getTodoFromString text = do
  let [id, title, content, deadline, isDoneStr] = lines text
  Todo (Id id) (Title title) (Content content) (Deadline deadline) (isDoneStr == "true")
    
getTodoFromFile :: FilePath -> IO Todo
getTodoFromFile path = do
  content <- readFile path
  return $ getTodoFromString content

writeToFile :: TodoList -> Todo -> IO Id
writeToFile (TodoList rootFolder) (Todo (Id id) (Title title) (Content content) (Deadline deadline) isDone) = do
  writeFile (rootFolder ++ "/" ++ id) $ intercalate "\n" [id, title, content, deadline, if isDone then "true" else "false"]
  return $ (Id id)

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo todoList title text deadline = do
  id <- getRandomId
  let todo = (Todo (Id id) title text deadline False)
  _ <- writeToFile todoList todo
  return $ (Id id)

readTodo :: TodoList -> Id -> IO Todo
readTodo (TodoList path) (Id id) = getTodoFromFile (path ++ "/" ++ id)

showTodo :: TodoList -> Id -> IO ()
showTodo todoList id = do
  (Todo (Id id) (Title title) (Content content) (Deadline deadline) isDone) <- readTodo todoList id
  putStrLn $ intercalate "\n" [id, title, content, deadline, if isDone then "true" else "false"]

removeTodo :: TodoList -> Id -> IO ()
removeTodo (TodoList rootFolder) (Id id) = removeFile $ rootFolder ++ "/" ++ id

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList id (TodoEdit title content deadline) = do
  todo <- readTodo todoList id
  _ <- writeToFile todoList todo {title=title, content=content, deadline=deadline}
  return ()

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList id = do
  todo <- readTodo todoList id
  _ <- writeToFile todoList todo {isDone=True}
  return ()

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo (TodoList rootFolder) = do
  files <- listDirectory rootFolder
  let filepaths = map (\filename -> rootFolder ++ "/" ++ filename) files
  todos <- mapM getTodoFromFile filepaths
  return $ sort todos

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  todos <- readAllTodo todoList
  return $ filter (\todo -> not (isDone todo)) todos

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = do
  todos <- readAllTodo todoList
  let _ = map (\(Todo id _ _ _ _) -> showTodo todoList id) todos
  return ()

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = do
  todos <- readUnfinishedTodo todoList
  let _ = map (\(Todo id _ _ _ _) -> showTodo todoList id) todos
  return ()

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}
tryGuessGame :: Integer -> IO ()
tryGuessGame answer = do
  putStr "Your number: "
  actual <- readLn :: IO Integer 
  case compare actual answer of
    GT -> do
      putStrLn "Too big"
      tryGuessGame answer
    LT -> do 
      putStrLn "Too small"
      tryGuessGame answer
    EQ -> putStrLn "Yep, that's the number!"

playGuessGame :: IO ()
playGuessGame = do
  answer <- randomRIO (0, 100)
  tryGuessGame answer

-- </Задачи для самостоятельного решения>