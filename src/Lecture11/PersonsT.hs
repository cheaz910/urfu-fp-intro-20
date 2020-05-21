{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture11.PersonsT where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (id)
import Lecture10.Reader (Person (..), PersonId, persons, processSingle, processPair)


-- <Задачи для самостоятельного решения>

{-
  В этом задании нужно адаптировать код из 10 лекции к новым требованиям
  при помощи трансформеров.

  1. Необходимо собирать статистику найденных людей:
    - количество одиноких персон
    - количество замужних персон
  2. В функцию `findById` добавить логгирование с помощью монады Write.
    Нужно сообщать была ли найдена персона по данному id или нет.

  Вы можете переиспользовать функции, которые остались без изменения из Lecture10.Reader
-}

data PersonSearchStats = PersonSearchStats
  { marriedPersonsCount :: Integer
  , singlePersonsCount :: Integer
  } deriving (Show)

emptyStats :: PersonSearchStats
emptyStats = PersonSearchStats 0 0

newtype PersonsT a = PersonsT
  { runPersonsT :: (ReaderT [Person] (StateT PersonSearchStats (Writer [String])) a) }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState PersonSearchStats
    , MonadReader [Person]
    , MonadWriter [String]
    )

runPersons :: PersonsT a -> ((a, PersonSearchStats), [String])
runPersons = runWriter . flip runStateT emptyStats . flip runReaderT persons . runPersonsT

findById :: PersonId -> PersonsT (Maybe Person)
findById pId = do
  persons' <- ask
  let findResult = find (\person -> id person == pId) persons'
  case findResult of
    Just person -> tell ["Found: " ++ show (id person)]
    _ -> tell ["Not found: " ++ show pId]
  return $ findResult

processPerson :: PersonId -> PersonsT (Maybe String)
processPerson pId = do
  put emptyStats
  person <- findById pId
  let spouseId = join $ marriedBy <$> person
  spouse <- case spouseId of
    (Just spouseId) -> findById spouseId
    _ -> return Nothing
  case person of
    Nothing -> return Nothing
    Just person' -> case spouse of
      Nothing -> do
        _ <- modify addSingleStat
        return $ Just $ processSingle person'
      Just spouse' -> do
        _ <- modify addMarriedStat
        return $ Just $ processPair person' spouse'
  where
    addSingleStat stats@(PersonSearchStats _ s) = stats {singlePersonsCount = s + 1}
    addMarriedStat stats@(PersonSearchStats m _) = stats {marriedPersonsCount = m + 1}

{-
  Функция должна выводить на экран:
  - общую поисковую статистику по всем найденым персонам.
  - результат каждого поиска

  Записывать в "persons.log" общий лог всех поисков.
-}
processPersons :: [PersonId] -> IO ()
processPersons personIds = do
  let ((results, stats), logs) = runPersons $ mapM (\p -> do
                                                      result <- processPerson p
                                                      return (p, result)
                                                   ) personIds
  mapM_ (\(id, result) -> putStrLn ("found: " ++ show id ++ "\n" ++ show result)) results
  putStrLn $ "stats:\n" ++ show stats
  writeFile "persons.log" $ show logs

-- </Задачи для самостоятельного решения>
