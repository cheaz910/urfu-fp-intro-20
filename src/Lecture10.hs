{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}

module Lecture10 where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Data.List

{-
  10: Остальные монады
-}

{- Класс Monad

    Вспомним, как выглядит класс типов Monad:

      class Monad m where
        (>>=)   :: m a -> (a -> m b) -> m b
        return  ::   a               -> m a
-}

{- Список

  В прошлый раз мы уже говорили, что список тоже эффект. Посмотрим, как можно использовать
  монадный синтаксис со списками на примере функции, которая возводит все элементы списка в квадрат.
-}

-- Примерно такое решение приходит в голову первым
sqrtList :: [Int] -> [Int]
sqrtList list = map (^2) list

{-
  Поймём, что нам нужно изменить, чтобы воспользоваться монадным синтаксисом.

    > :t map
    map :: (a -> b) -> [a] -> [b]

  Cравним с (>>=)

    > :t (>>=)
    (>>=) :: Monad m => m a -> (a -> m b) -> m b

  Во-первых, нам нужно поменять местами аргументы функции у (>>=).

    > :t (=<<)
    (=<<) :: Monad m => (a -> m b) -> m a -> m b

  Эта функция точно такая же как (>>=), но с другим, более подходящим для нас порядком
  аргументов. Последнее отличие

    (=<<) :: Monad m => (a -> m b) -> m a -> m b
      map ::            (a -> b) ->   [a] -> [b]
                              ^
          Функция должна возвращать уже завёрнутое значение

  В стандартной библиотеке уже есть такая функция — concatMap.

    > :t concatMap
    concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

  Снова сравним:

    (=<<)     :: Monad m    => (a -> m b) -> m a -> m b
    concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
-}

-- Заменим map на concatMap в sqrtList
sqrtListConcatMap :: [Int] -> [Int]
sqrtListConcatMap list = concatMap (\x -> [x^2]) list

-- Можно переписать функцию с инфиксной записью concatMap:
sqrtListConcatMapInfix :: [Int] -> [Int]
sqrtListConcatMapInfix list = (\x -> [x^2]) `concatMap` list

-- Осталось переписать эту функцию с помощью монадных операций:
sqrtListMonad :: [Int] -> [Int]
sqrtListMonad list = (\x -> return $ x^2) =<< list

-- Точно так же к списку можно применить монадный синтаксический сахар
sqrtListDoNotation :: [Int] -> [Int]
sqrtListDoNotation list = do
  x <- list
  return $ x^2

-- А теперь вспомним синтаксис list comprehensions
sqrtListComprehensions :: [Int] -> [Int]
sqrtListComprehensions list = [ x^2 | x <- list ]
-- На самом деле, это синтаксический сахар, который разворачивается в запись sqrtListDoNotation,
-- которая в свою очередь использует (=<<)


-- <Задачи для самостоятельного решения>

{-
  Напишите функцию, которая строит декартово произведение двух списков,
  т.е. список всевозможных пар (x,y), где x — из первого списка, а y — из второго.
-}

-- "Наивная" версия с использованием map'ов
cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs [] = []
cartesianProduct [] ys = []
cartesianProduct (x:xs) ys = map ((,) x) ys ++ cartesianProduct xs ys

-- По аналогии с функцией sqrtList перепишите cartesianProduct с использованием return и (=<<)
cartesianProductMonad :: [a] -> [b] -> [(a,b)]
cartesianProductMonad xs ys = (\x -> (\y -> return (x, y)) =<< ys) =<< xs 

-- А теперь с использованием do notation
cartesianProductDoNotation :: [a] -> [b] -> [(a,b)]
cartesianProductDoNotation xs ys = do
  x <- xs
  y <- ys
  return (x, y)

-- </Задачи для самостоятельного решения>

{- Applicative Functor

  Снова обратимся к определению класса Monad

    > :i Monad
    class Applicative m => Monad (m :: * -> *) where

  Мы ещё не обсуждали класс Applicative. Посмотрим, как определяется он:

    > :i Applicative
    class Functor f => Applicative (f :: * -> *) where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b
      ...

  Напомним, как выглядит класс Functor — класс типов, содержимое которых можно
  преобразовать с помощью какой-то функции.
  > :i Functor
    class Functor (f :: * -> *) where
      fmap :: (a -> b) -> f a -> f b

  Значит Applicative это класс уже знакомых нам функторов, значит
  к нему можно применить функцию fmap или (<$>). Появились две новые функции:
  - `pure` берёт значение и складывает его в функтор
  - (<*>) очень похожа на fmap, но теперь функция, которую мы хотим
  использовать тоже содержится в функторе.

  Например для IO:

    instance Applicative IO where
      pure = return
      a <*> b = do
          f <- a
          x <- b
          return (f x)

  Рассмотрим пример использования (<*>) для списков
-}

-- Чтобы применить функцию к каждому элемента списка можно воспользоваться fmap
sqrs :: [Int]
sqrs = fmap (^2) [1..10]  -- или (^2) <$> [1..10]

{-
  А что делать, если нам нужно применить список функций к элементу или списку элементов?
  Напишем функцию, которая будет по точке возвращать её и всех её соседей.
-}

-- Функция сложения двух точек
plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Сдвиги для получения всех соседей  [(1, 1), (1, 0), ... (0, -1), (-1, -1)]
shifts :: [(Int, Int)]
shifts = concat $ map (\x -> zip coordShifts $ repeat x) coordShifts
  where
    coordShifts = [(-1), 0, 1]

-- Сформируем массив функций, каждая из которых применяет свой сдвиг к точке
neighborsFuncs :: [(Int, Int) -> (Int, Int)]
neighborsFuncs = map plus shifts

-- Осталось реализовать функцию, которая применит каждую функцию к точке и получит её соседей
neighbors :: (Int, Int) -> [(Int, Int)]
{-
  Для этого нам нужна функция вот с таким типом:

    f :: [Point -> Point] -> Point -> [Point]

  Или, если мы хотим получить соседей для нескольких точек сразу:

    f :: [Point -> Point] -> [Point] -> [Point]

  Что в точности соответствует функции (<*>)

    (<*>) :: f (a -> b) -> f a -> f b
-}
neighbors x = neighborsFuncs <*> [x]

{-
  Аппликативный функтор даёт нам возможность применять функцию, которая сама
  находится в этом функторе. Почему это может быть полезно?

  Допустим у нас есть чистая функция (++) и мы хотим частично применить её к строке
  Just "johntra", чтобы потом где-то использовать. Понятно, что просто передать её вот так

    (++) (Just "johntra")

  мы не можем, т.к. Maybe не список. Зато мы можем применить её с помощью fmap

    (++) <$> Just "johntra"

  Но в результате функция оказалась внутри Maybe

    > :t (++) <$> Just "johntra"
    (++) <$> Just "johntra" :: Maybe ([Char] -> [Char])

  И просто так применить её к Just "volta" мы не можем. Здесь нам и поможет <*>

    > (++) <$> Just "johntra" <*> Just "volta"
    Just "johntravolta"
-}

{-
  Снова посмотрим на класс Monad

    > :i Monad
    class Applicative m => Monad (m :: * -> *) where
      (>>=) :: m a -> (a -> m b) -> m b
      return :: a -> m a
      { - # MINIMAL (>>=) # - }

  И теперь мы знаем всё, что нужно, чтобы делать собственные монады!
-}

-- <Задачи для самостоятельного решения>

{-
  Реализуйте instance класса Monad для Optional
-}

-- Maybe другими словами
data Optional a = Some a | None deriving (Eq, Show, Functor)

{-
  Обратите внимание, что instance для Functor выведется автоматически
  с помощью deriving (Functor) и специальных расширений.

    instance Functor Optional where
      fmap f = \case
        Some x -> Some $ f x
        None -> None
-}

instance Applicative Optional where
  (Some x) <*> (Some y) = Some (x y)
  None <*> _ = None
  _ <*> None = None
  pure x = Some x


instance Monad Optional where
  Some a >>= f = f a
  None >>= _ = None

{-
  Реализуйте instance класса Monad для List
-}

data List a = Nil | a :. (List a) deriving (Eq, Show)

infixr 5 :.

instance Functor List where
  fmap f = \case
    Nil -> Nil
    (:.) x xs -> (:.) (f x) (fmap f xs)

-- понадобится для <*>
append :: List a -> List a -> List a
append Nil ys = ys
append ((:.) x xs) ys = (:.) x (append xs ys)

instance Applicative List where
  f :. fs <*> xs = append (f <$> xs) (fs <*> xs)
  _ <*> _ = Nil
  pure x = x :. Nil

-- понадобится для >>=
concat' :: List (List a) -> List a
concat' Nil = Nil
concat' ((:.) x xs) = append x (concat' xs)

instance Monad List where
  (>>=) xs f = concat' (fmap f xs)

-- </Задачи для самостоятельного решения>

{- Writer

  Часто при решении задач из функции нужно вернуть несколько значений.
  Простой пример: пусть есть функция f :: a -> b и мы хотим помимо основого значения получать
  дополнительную информацию, например, для отладки. Мы можем возвращать пару:
-}

parseInt :: String -> (Int, String)
parseInt x = (read x, "Parse '" ++ x ++ "' as Int")

repeatInt :: Int -> ([Int], String)
repeatInt x = (repeat x, "Repeat '" ++ show x ++ "' infinitely")

{-
  Но соединенить такие функции с помощью композиции уже не выйдет:

    parseInt  :: String -> (Int, String)
    repeatInt :: Int -> ([Int], String)

    > let h = repeatInt . parseInt
    <interactive>:6:13: error:
    * Couldn't match type `(Int, String)' with `Int'
      Expected type: String -> Int
        Actual type: String -> (Int, String)

  Чтобы их соединить нужно:
  - взять результат первой функции
  - выделить из него собственно результат вычислений
  - передать его на вход второй функции
  - получить ее результат
  - соединить отладочную информацию обеих функций
  - вернуть новую пару

  На самом деле, нам снова нужно взять что-то "завёрнутое", развернуть, как-то это обработать,
  а потом завернуть обратно. Конечно, нам нужна монада. Для этого в Haskell есть монада Writer.

  Чтобы посмотреть, как она реализуется загляните в "src/Lecture10/WriterImplementation.hs"
-}

parseWithLog :: String -> Writer String Int
parseWithLog x = do
  tell ("Parse '" ++ x ++ "' as Int\n")     -- tell :: l -> Writer l ()
  return $ read x

repeatWithLog :: Int -> Writer String [Int]
repeatWithLog x = do
  tell ("Repeat '" ++ show x ++ "' infinitely\n")
  return $ repeat x

parseAndRepeat :: String -> Writer String [Int]
parseAndRepeat x = parseWithLog x >>= repeatWithLog
{-
  Ещё можно определить `parseAndRepeat` так:

    parseAndRepeat =  parseWithLog >=> repeatWithLog

  где

    > :t (>=>)
    (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

  И, конечно, с do-notation

    parseAndRepeat x = do
      y <- parseWithLog x
      repeatWithLog y
-}

-- А можно использовать значение "молча"
parseAndReplicate :: Int -> String -> Writer String [Int]
parseAndReplicate n x = take n <$> parseAndRepeat x

-- Функция runWriter возвращает пару (value, log)
getLog :: Writer String [Int] -> String
getLog = snd . runWriter
{-
  Lecture10> getLog $ parseAndRepeat "10"
  "Parse '10' as Int\nRepeat '10' infinitely\n"
-}

{-
  Вообще говоря, дополнительное значение не обязано быть строкой, но и произвольный тип иметь не может.
  Посмотрим на пример выше и попробуем найти ограничения на тип дополнительной информации.
  Оно скрыто в фразе "соединить отладочную информацию обеих функций", т.е. тип должен быть таким,
  чтобы его значения можно было соединить. Так же требуется какое-то "нулевое" значение,
  с которого начнётся накопление дополнительной информации. Нам нужен Monoid.

  Почему это работает для строк?
  Как мы знаем, тип String синоним для [Char]. Для списка операцией соединения будет (++),
  а нейтральным элементом — [].

    newtype Writer l a = Writer { runWriter :: (a, l) }

    instance (Monoid l) => Monad (Writer l) where
        return a = Writer (a, mempty)
        (>>=) (Writer (a, l)) g = Writer (b, mappend l l')
            where
                Writer (b, l') = g a

    tell :: Monoid l => l -> Writer l ()
    tell s = Writer ((), s)
-}

{- State

  Здесь всё просто, монада State позволяет хранить состояние вычислений, что нельзя сделать
  просто так, ведь все переменные не меняются. `State s a` представляет собой вычисление, которое
  использует состояние типа `s` и возвращает результат типа `a`.
-}

-- Напишем функцию, подсчитывающую количество раз, которое символ встретился в строке
countChar :: Char -> String -> State Int String
countChar c [] = do
    count <- get                                -- get :: State s a => State s a возвращает текущее состояние
    return $ "Count of " ++ show c ++ " is " ++ show count

countChar c (x:xs) = do
    count <- get
    put (if x == c then count + 1 else count)   -- put :: State s a => s -> State s () String обновляет состояние
    countChar c xs

-- execState :: State s a -> s -> s
countOfN :: Int
countOfN = execState (countChar 'n' "anna") 0

-- runState :: State s a -> s -> (a, s)
printCountOfA :: IO ()
printCountOfA = do
  let countOfA = fst $ runState (countChar 'a' "anna") 0
  putStrLn countOfA

-- Функция modify :: State s a => (s -> s) -> State s () изменяет состоянии с помощью переданной функции
modifyExample :: Int
modifyExample = snd $ runState (do
    modify (+1)
    modify (*2)
    modify (+3)
  ) 5

{- Reader

  Допустим у нас есть некоторое значение, которое нужно для работы нескольких функций.
  Но значение это меняется в зависимости от окружения, а значит нельзя вычислить его
  на этапе компиляции. Например, конфиг.

  Например, пусть у нас есть конфиг и функции, которые с его помощью строят список
  всех доступных в API url'ов.
-}

data Config = Config
  { port :: Int
  , host :: String
  -- ...
  } deriving (Eq, Show)

buildRootUrl :: Config -> String
buildRootUrl config = host config ++ ":" ++ show (port config) ++ "/"

buildUserApiUrl :: Config -> String
buildUserApiUrl config = buildRootUrl config ++ "users/"

buildAdminApiUrl :: Config -> Int -> String
buildAdminApiUrl config version = buildRootUrl config ++ "admin/" ++ show version ++ "/"

buildMap :: Config -> Int -> String
buildMap config version = intercalate "\n" [root, userApi, adminApi]
  where
    root = buildRootUrl config
    userApi = buildUserApiUrl config
    adminApi = buildAdminApiUrl config version

{-
  У этого кода есть несколько особенностей
  - Все функции зависит от конфига
  - У них могут быть разные аргументы после, но они возвращают одно и то же
  - Предполагается, что все функции получили одно и то же значение в качестве конфига
  - Функция buildMap сама не использует config, а только его прокидывает другим функциям

  Кажется, что нам здесь нужно что-то вроде неизменяющегося State, т.е. мы хотим
  незаметно таскать за собой состояние, но при этом не хотим его менять.

  Для такой задачи есть монада `Reader e a`, где `e` представляет собой окружение, в котором
  производится вычисление, в нашем случае конфиг, а `a` — результат этого вычисления.

    newtype Reader e a = Reader { runReader :: e -> a }

  С помощью Reader мы можем переписать код вот так:
-}

buildRootUrlR :: Reader Config String
buildRootUrlR = do
  config <- ask
  return $ "/" ++ host config ++ ":" ++ show (port config) ++ "/"

buildUserApiUrlR :: Reader Config String
buildUserApiUrlR = do
  root <- buildRootUrlR
  return $ root ++ "users/"

buildAdminApiUrlR :: Int -> Reader Config String
buildAdminApiUrlR version = do
  root <- buildRootUrlR
  return $ root ++ "admin/" ++ show version ++ "/"

buildMapR :: Int -> Reader Config String
buildMapR version = do
  root <- buildRootUrlR
  userApi <- buildUserApiUrlR
  adminApi <- buildAdminApiUrlR version
  return $ intercalate "\n" [root, userApi, adminApi]

-- Чтобы запустить Reader нужно воспользоваться функцией runReader :: Reader e a -> e -> a
showMap :: IO ()
showMap = putStrLn $ runReader (buildMapR 2) (Config 8088 "localhost")

{-
  Мы опять можем описать Reader и в терминах вычислений.
  Reader — вычисление, результатом которого является тип `a`,
  а чтобы этот результат получить нам нужен тип `e`.

  Не напоминает ли это вам какой-нибудь другой тип?
  Конечно же, это функция (e -> a). Убедимся в том, что это так,
  для этого немного поиграем с типов функций

    buildRootUrl      :: Config -> String
    buildMap          :: Config -> Int -> String

  Последнюю можно переписать вот так:

    buildMap          :: Int -> Config -> String

  А т.к. (->), как и всё в Haskell, функция, можно переписать их вот так:

    buildRootUrl      ::        ((->) Config String)
    buildMap          :: Int -> ((->) Config String)

  Что удивительно похоже на типы функция с Reader'ом:

    buildRootUrlR     :: Reader Config String
    buildMapR         :: Int -> Reader Config String

  Всё дело в том, что Reader просто синоним понады ((->) r). Убедимся в том, что ((->) r) — монада.
  Для этого нужно определить instance класса Monad, Applicative и Functor. Сделаем это

    Class Functor f where
      fmap :: (a -> b) -> f a -> f b

  Подставим в тип fmap ((->) r)

    fmap :: (a -> b) -> (((->) e) a) -> (((->) e) b)

  И перепишем в более привычном для нас виде

    fmap :: (a -> b) -> (e -> a) -> (e -> b)

  Посмотрим внимательно, вдруг нам это что-то напомнит. Да это просто же композиция:

    fmap f g x = f (g x)

  Идём дальше и поступаем аналогично

    class Functor f => Applicative (f :: * -> *) where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

    pure :: a -> f a            ~>
            a -> (((->) e) a)   ~>
            a -> e -> a         ~>
            \x _ -> x           ~>
    pure  = const

    (<*>)     :: f (a -> b) -> f a -> f b                             ~>
                (((->) e) (a -> b)) -> (((->) e) a) -> (((->) e) b)   ~>
                ((e -> (a -> b)) -> (e -> a) -> (e -> b)              ~>
                (e -> a -> b) -> (e -> a) -> e -> b                   ~>
    (<*>) f g = \e -> f e (g e)

  И наконец Monad

    class Monad m where
      return  :: a -> m a
      (>>=)   :: m a -> (a -> m b) -> m b

    return  :: a -> m a   ~>
    pure    :: a -> f a   ~>
    return  = const

    (>>=) :: m a -> (a -> m b) -> m b                              ~>
            (((->) e) a) -> (a -> (((->) e) b)) -> (((->) e) b)    ~>
            (e -> a) -> (a -> (e -> b)) -> (e -> b)                ~>
            (e -> a) -> (a -> e -> b) -> e -> b

  Взглянем на (<*>)

    (<*>) ::(e -> a -> b) -> (e -> a) -> e -> b

  Видно, что (>>=) это про (<*>) с поменятыми местами первым и вторым аргументами

    (>>=) f g = flip f <*> g

  Но можно и написать определение с нуля

    (>>=) f g = \e -> g (f e) e

  Получается, что Reader просто синоним монады ((->) r) с дополнительными функциями
  для удобного использования.
-}

-- <Задачи для самостоятельного решения>
-- Вы найдёте в src/Lecture10/Reader.hs
-- </Задачи для самостоятельного решения>

{- Ссылки

  - What a Monad is not               http://wiki.haskell.org/What_a_Monad_is_not
  - Тройка полезных монад             http://habr.com/en/post/184722/
  - The Reader Monad — Part 1         https://hackernoon.com/the-reader-monad-part-1-1e4d947983a8
  - The State Monad!                  https://www.seas.upenn.edu/~cis552/19fa/lectures/stub/Monads2.html
  - Functional Parsers                https://www.youtube.com/watch?v=OrAVS4QbMqo&list=PLoJC20gNfC2gpI7Dl6fg8uj1a-wfnWTH8&index=8
  - Parsing with Applicative Functors https://www.seas.upenn.edu/~cis552/19fa/lectures/stub/Parsers.html
  - The Maybe and List Monads         https://www.seas.upenn.edu/~cis552/19fa/lectures/stub/Monads.html
-}