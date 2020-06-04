{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Lecture13 where

import Data.Kind

{-
  13

  - SafeList
  - Рассказать HList
    - Проблема: хотим гетерогенный список
    - Как сделать?
  - Введение про термы, типы
  - TypeLits
    - https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-TypeLits.html
    - :> "api" :> "sda"
  - Показать printf
    - idris: dependent types облегчают работу
    - haskell: печально
-}

{-
  Программирование на уровне типов далеко не первое, что может понадобится
  при создании программы. Большинство программистов вообще не испытывают такой
  потребности в том числе потому, что далеко не все языки вообще позволяют
  это делать.

  В C# можно совершать какие-то манипуляции с типам с помощью Reflection, Expressions
  и даже генерировать IL-код. Но эти инструменты работают только в runtime.
  Мы же будет говорить о программе на уровне типов,которая будет выполняться
  во время компиляции, т.е. её результатом будет скомпилированная программа
  или ошибка компиляции.

  Что вообще значит "программирование на уровне типов"?
  Type-level программа — это программа, которая в качестве данных для обработки
  использует типы. Мы уже видели самые примитивные версии таких программ.
  Например, когда мы создаём полиморфный тип на Haskell:

    data Maybe a = Nothing | Just a

  Или generic класс на C#:

    class Maybe<T> where T : struct { ... }

  Мы пишем код, который использует переменную типа. Т.е. мы создаём тип,
  который, принимая на вход тип в качеcтве переменной, создаёт новый тип.
  Точно так же, как функция (+), принимая на вход два числа, возвращает
  новое число.

  Зачем нам вообще может это понадобится?
  Использовать программирование на уровне типов можно по-разному.
  Например, полиморфный код (частный случай type-level programming) позволяет
  нам сократить количество написанного кода. Сейчас мы посмотрим, как с его
  помощью можно перевести ошибки runtime в ошибки компиляции.
-}

{- Безопасная голова

  Как мы уже говорили, многие стандартные функции часто не используют в production
  проектах потому, что они небезопасные, т.к. могут выкинуть исключение.

  Напрмер, функция head бросает исключение для пустого списка. Это можно поправить,
  написав свою функцию-обёртку, которая будет проверять список на пустоту и, например,
  возвращать Maybe а.

  Мы попробуем поступить по-другому. Попробуем сделать функцию safeHead, которая
  не будет компилироваться, если её вызывают от пустого списка.
-}

-- Нам нужно придумать, как изменить тип этой функции так, чтобы программа,
-- содержащая `safeHead []` не компилировалась
safeHeadList :: [a] -> a
safeHeadList = undefined -- ???

-- Подумайте, как можно это сделать?

-- Наверное, первое, что приходит в голову, просто убрать Nil из списка,
-- т.е. запретить пустой список вообще.
data NonEmptyList a = a :| [a]

-- ^ Такой тип называется `NonEmpty` и лежит в Data.List.NonEmpty в пакете semigroups

safeHeadNEL :: NonEmptyList a -> a
safeHeadNEL (a :| _) = a

{-
  У такого решения есть одна проблема — мы действительно не можем создать
  пустой список. Но ведь нам иногда нужно вернуть пустой список,
  например, мы запросили список заказов и заказов нет. Или применили
  к списку фильтр и ни один элемент не подошёл и т.д. и т.п.

  Получается нам нужен ещё один тип, который всё-таки может быть пустым,
  но который будет содержать в себе нашу непустую версию, чтобы мы могли
  писать безопасные функции. Сделаем такой тип:
-}

data SafeListAttempt a = EmptyList | NonEmpty (NonEmptyList a)

{-
  Новая проблема — непонятно, как теперь объяснить типу функции safeHead,
  что нам нужны только непустые списки. Мы не можем использовать
  конструктор в типе функции.

    safeHeadSLA :: NonEmpty (NonEmptyList a) -> a       -- ???

  Единственный выход — использовать функцию, определённую выше,
  но перед этим pattern match'ить конструкторы SafeListAttempt,
  т.е. делать то же, что мы бы делали для обычного списка.
-}
someFunc :: SafeListAttempt a -> Maybe a
someFunc EmptyList = Nothing
someFunc (NonEmpty list) = Just (safeHeadNEL list)

{-
  Первая попытка вышла неудачной. Но мы поняли основную идею.
  Она заключается в том, чтобы с помощью типа отделить пустой
  список от непустого. Если у нас это получится, то пустой список
  будет иметь тип отличный от непустого списка и при его передаче
  в безопасную функцию, код не скомпилируется.

  Хорошо, раз мы не можем использовать конструктор в типе функции,
  тогда создадим специальные типы, которыми и будем помечать
  пустоту списка:
-}

-- Типы без конструкторов, нельзя создать данные такого типа.
-- Существуют они только на этапе компиляции.
data Empty
data NonEmpty

-- Параметр b как раз и будет содержать тип-метку
-- SafeList' a NonEmpty — непустой
-- SafeList' a Empty — пустой
data SafeList' a b = Nil' | Cons' a (SafeList' a b)

-- Теперь мы действительно можно написать тип функции,
-- которая не будет компилироваться, если ей передать пустой список:
safeHead' :: SafeList' a NonEmpty -> a
safeHead' (Cons' a _) = a
safeHead' Nil' = undefined

{-
  Однако у такой функции есть одна проблема. Она получилось частично
  определённой, т.е. она обрабатывает не все pattern match'и.
  И совершенно непонятно, как написать `safeHead' Nil'`, ведь нам нужно
  вернуть значение типа `a`, а взять его неоткуда.
  К сожалению, частично определённый программы в Haskell по умолчанию
  компилируются, хотя во многих других функциональных языках они запрещены.

  Но всё-таки такие функции плохи, опять же, тем, что могут выкинуть исключение
  в runtime, а это как раз то, от чего мы и пытаемся избавиться.
  К тому же, очень многие Haskell-разработчики используют -fwarn-incomplete-patterns,
  который запрещает частно определённые программы, он, кстати, включен в проекте
  нашего курса. Так что такая программа просто не скомпилируется.

  Почему же у нас не получилось "отсеить" Nil' в типе так, чтоб стало ясно, что нам
  не нужно разбирать match с Nil'? Ответ на этот вопрос дадут типы конструкторов:

    Nil' :: SafeList' a b
    Cons' :: a -> SafeList' a b -> SafeList' a b

  Оба конструктора возвращают `SafeList' a b`, т.е. у нас всё-таки не получилось
  объяснить Haskell'у, что Cons' возвращает непустой список, а Nil' — пустой.
  Нам же хочется получить что-то такое:

    Nil' :: SafeList' a Empty
    Cons' :: a -> SafeList' a b -> SafeList' a NotEmpty

  Но написать так при объявлении типа нельзя. Справиться с этим на поможет специальный
  оператор (~)

    (~) :: * -> * -> Constraint

  Как видно из типа, он принимает в качестве аргумента два типа и возвращает constraint,
  который требует, чтобы первый тип был эквивалентен второму.
-}

-- Фактически это то же, что и `someNum :: Int`, но написали мы это с помощью contraint'а
--  Lecture13> :t someNum
--  someNum :: Int
someNum :: a ~ Int => a
someNum = 3

{-
  Этот оператор определён в GHC.Types. Для него выполняются следующие правила:
    - Реклексивность:  a ~ a
    - Симметричность:  a ~ b && b ~ a
    - Транзитивность:  a ~ b, b ~ c => a ~ c
  Другими словами, является отношением эквивалентности.

  Ну и с помощью этого оператора мы можем решить нашу задачу, ведь мы можем
  писать contraint'ы при объявлении конструторов:
-}

data SafeListEq a b
  = b ~ Empty => NilEq
  | forall c . b ~ NonEmpty => ConsEq a (SafeListEq a c)

{-
  Нам пришлось явно "объявить" `c` и `b` с помощью forall, чтобы явно показать,
  что ConsEq в качестве аргумента принимает любой список `SafeListEq a c`,
  но вот возвращает обязательно непустой.
  Типы конструкторов получились ровно такими, как нам было нужно:

    NilEq :: SafeListEq a Empty
    ConsEq :: a -> SafeListEq a c -> SafeListEq a NonEmpty
-}

-- И теперь `SafeListEq a NonEmpty` можно сконструировать
-- только с помощью ConsEq, а значит функция определена полностью.
safeHeadEq :: SafeListEq a NonEmpty -> a
safeHeadEq (ConsEq a _) = a

{-
  Всё то же самое можно написать гораздо проще, если использовать
  расширение для Generalised Algebraic Data Types (GADTs).
  Оно позволяет явно указывать типы конструкторов при объявлении,
  а именно этого мы пытались добиться с помощью (~).
  Так мы можем выразить список с меткой о пустоте с помощью GADs:
-}

data SafeList a b where
  Nil  :: SafeList a Empty                          -- Возвращаемый тип конструктра
  Cons :: a -> SafeList a b -> SafeList a NonEmpty  -- Может отличаться от `SafeList a b`

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons a _) = a

{-
  Можно опробовать нашу функцию в ghci:

    Lecture13> safeHead $ Cons 3 $ Nil
    3

    Lecture13> safeHead Nil
    <interactive>:8:10: error:
        * Couldn't match type `Empty' with `NonEmpty'
          Expected type: SafeList a NonEmpty
            Actual type: SafeList a Empty
-}


{- Гетерогенный список

  Почти во всех современных языках есть таплы и списки. Список содержит
  произвольное количество элементов одного типа, тапл — определённое количество
  элементов разных типов. Почему же нет типа, который бы описывал структуру,
  содержащую произвольное количество элементов разных типов?

  Ответ простой — совершенно непонятно, как такой тип записать. Если элементов
  может быть хоть сколько и типы этих элементов могут быть произвольными, мы
  просто не можем взять и перечислить типы всех элементов, как это сделано
  в тапл, или описать содержимое одним типом, как это сделано в списке.

  Список, содержащий элементы разных типов называется гетерогенным.
  Список, содержащий элементы одного типа называется гомогенным.

  Примеры правильно типизированного гетерогенного списка:
  - [1, "string", 3.4] :: [Int, String, Double]
  - [True, "string", 1, 3.4] :: [Bool, String, Int, Double]

  Примеры неправильно типизированного гетерогенного списка:
  -  ["string", 1, 3.4] :/: [Int, String, Double]

  Понятно, что в целом можно сделать такой список в C#, например, складывая
  все элементы в список object'ов (или dynamic), запоминая каким-то образом
  тип элемента и при его получении приводить object к этому типу. И понятно,
  чем это плохо. В случае ошибки при приведении типов возникнет исключение.

  На самом деле будут только вести себя как гетерогенный список, но внутри
  использовать гомогенные списки List<object>, List<dynamic>. Т.е. такие решения
  по сути своей не будут строго типизированными.

  Мы же хотим сделать строго типизированный гетерогенный список.
  Для выражения такого типа нам нужно уметь по списку типов элементов в списке
  генерировать тип самого списка. Это можно выразить в помощью GADTs вот так:
-}

data HList (xs :: [*]) :: * where  -- с помощью kind'ов мы сказали, что нам нужен
  HNil  :: HList '[]               -- список простых типов
  HCons :: x -> HList xs -> HList (x ': xs)

{-
  Обратите внимание на символ ', который мы уже видели в описании servant API.
  Этот символ поднимает значение на уровень типов, он как бы указывает на то,
  что тип нужно воспринимать как значение, которое используется для задания типа.
  Например, [Int] — тип списка чисел, а '[Int] — тип, описывающий список,
  в котором лежит ровно один Int, как [3] — список, в котором лежит число 3.
  Именно с помощью такого символа мы можем описать тип геторогенного списка.

  При этом важно, что хоть мы здесь и записали очень конкретный список,
  который, в целом, можно заменить и на Tuple (ведь мы заранее знаем типы элементов),
  мы можем конструировать значения типа HList с произвольными типами внутри.
  Попробуйте сами в ghci.
-}
hList :: HList '[Int, [Char], Bool]
hList = HCons 3 $ HCons "string" $ HCons True $ HNil

{-
  К сожалению, вызвать `hList` в ghci у нас не получится, т.к. у него нет instance'а
  Show, и вывести его автоматически не получится, т.к. это не работает для GADs:

    Lecture13.hs:292:13: error:
    * Can't make a derived instance of `Show (HList xs)':
        Constructor `HNil' is a GADT
        Constructor `HCons' is a GADT

  Значит нам нужно написать instance Show самостоятельно. Сделать это можно примерно так:

    instance Show (HList xs) where
      show = \case
        HNil -> "nil"
        (HCons x xs) -> show x ++ " : " ++ show xs

  Но у нас сразу возникенет проблема. В HList могут содержать элементы произвольных типов,
  а значит нам нужно явно указать, что для каждого типа должен существовать instance Show.
  Для этого нет стандартного contraint'а, нам нужно научиться конструировать свой,
  как мы это делали в Lecture12 для MonadDB, но на этот раз для произвольного списка типов,
  а значит просто написать type = (Contraint1, Contraint2, ... уже не получится, нам нужна
  функция на уровне типов. Такую функцию на Haskell можно написать с помощью семейства типов
  `type family`. В целом type family можно воспринимать как способ объявления функции,
  но не для значений, а для типов.
-}

-- Явно указываем, что принимаем на вход список типов xs :: [Type].
-- Тоже можно сделать так: xs :: [*].
-- Type — просто более понятный синоним *       Явно указали, что конструируем Constraint
-- type Type = *         ↓                    ↙
type family ShowAll (xs :: [Type]) :: Constraint where
  ShowAll '[] = ()
  ShowAll (x ': xs) = (Show x, ShowAll xs)
  --                ^ (Show x, (Show y, Show z)) ~> (Show x, Show y, Show z)

{-
  В примере выше мы воспользовались закрытым (closed) семейством типов. Это значит, что мы
  не сможем добавить к этому семеству ещё один тип в другом месте программы.
  Помимо этого есть открытые семесйтва типов. Можно переписать код выше вот так:

    type family ShowAll (xs :: [Type]) :: Constraint
    type instance ShowAll '[] = ()
    type instance ShowAll (x ': xs) = (Show x, ShowAll xs)

  Тогда можно будет добавить `type instance ShowAll` где угодно ниже в модуле или
  в другом модуле, если семейство типов будет импортированно.

  Теперь мы можем воспользоваться constraint'ом ShowAll и написать instance Show для HList:
-}

instance ShowAll xs => Show (HList xs) where
  show = \case
    HNil -> "nil"
    (HCons x xs) -> show x ++ " : " ++ show xs

-- <Задачи для самостоятельного решения>

-- Реализуйте instance Eq для HList
type family EqList (xs :: [Type]) :: Constraint where
    EqList (x ': xs) = (Eq x, EqList xs)
    EqList _ = ()

instance EqList xs => Eq (HList xs) where
    (HCons x xs) == (HCons y ys) = x == y && xs == ys
    HNil == HNil = True

-- </Задачи для самостоятельного решения>

{- Servant explained

  Теперь у нас есть всё, чтобы разобраться в том, как же работает servant.
  Без ограничения общности, мы рассмотрим чуть упрощённую версию (без имён
  захваченных из url'а параметров).
  В нашем проекте из Lecture12 есть вот такое объявление API:

    type PreliminaryAPI
      = "api" :> "movie_sessions"
        :> Capture MovieSessionId
        :> "preliminary_booking"
        :> Capture SeatId
        :> Get BookingId

  И servant использует это объявление для того, чтобы
  - Определить и проверить типы обработчиков запросов
  - Составить url'ы API.

  Посмотрим, как сделана первая часть (вам не составит труда разобраться
  со второй частью самостоятельно, если будет желание, см. ссылки).

  Сначала поймём, что нам нужно. Мы хотим из типа PreliminaryAPI
  выкинуть строковые литералы, а из всех Capture и Get/Post/... соорудить
  тип обработчика. Нам снова нужна функция на уровне типов.
  Назовём её Server. Вот, что она должна получить из PreliminaryAPI:

  Server PreliminaryAPI                ~ [ распишем PreliminaryAPI по определению ] ~

  ~  Server ("api"                     ~ [ сначала обработаем первый аргумент (:>)] ~
            :> "movie_sessions"
            :> Capture MovieSessionId
            :> "preliminary_booking"
            :> Capture SeatId
            :> Get BookingId)

  ~  Server ("api") :>                  ~ [ строковые литералы нам не нужны,
     Server ("movie_sessions"               выкинем, то же с "movie_sessions"     ] ~
            :> Capture MovieSessionId
            :> "preliminary_booking"
            :> Capture SeatId
            :> Get BookingId)

  ~  Server (Capture MovieSessionId     ~ [ Обработаем первый аргумент            ] ~
            :> "preliminary_booking"
            :> Capture SeatId
            :> Get BookingId)

  ~  Server (Capture MovieSessionId) :> ~ [ Если обработчик принимает в качестве
     Server ("preliminary_booking"          первого аргумента типа MovieSessionId,
            :> Capture SeatId               значит тип обработчика обязательно
            :> Get BookingId)               начинается с `MovieSessionId ->`      ] ~

  ~  MovieSessionId ->                  ~ [ Снова выкинем строковый литерал       ] ~
     Server ("preliminary_booking") :>
            :> Capture SeatId
            :> Get BookingId)

  ~  MovieSessionId ->                  ~ [ Раз обработчик принимает аргумент типа
     Server (Capture SeatId) :>             SeatId, значит в его типе будет
            :> Get BookingId)               `SeatId ->`, так и запишем             ] ~

  ~  MovieSessionId ->                  ~ [ Во-первых, мы понимаем, что обработчик
     SeatId         ->                      должен возвращать BookingId, во-вторых,
            Server (Get BookingId)          он работает с вводом/выводом, значит
                                            нам нужна монада IO                    ] ~

  ~  MovieSessionId ->                  ~ [ И мы получили тип нашего обработчика
     SeatId         ->                      postPreliminary c точностью до AppT    ]
     IO BookingId

  Теперь просто запишем всё, что нам нужно с помощью семейства типов:

    type family Server layout :: * where
      Server (a :<|> b) = Server a :<|> Server b
      Server ((s :: Symbol) :> r) = Server r
      Server (Capture a :> r) = a -> Server r
      Server (Get a) = IO a

  Дальнешие потребности можете узнать из ссылок в конце лекции.
-}

{- value :: Type :: * :: ?

  Подумаем, что у нас получилось.

  В начале курса мы познакомились с нетипизированным лямбда исчислением,
  чуть позже мы добавили к нему типы. Получился вполне работоспособный
  язык программирования, но едва ли нам захотелось на нём писать, ведь
  в нём не хватало очень важной вещи — полиморфизма. Чтобы добавить его
  мы воспользовались абстракцией и апликацией, который уже были определены
  для значений, но применили их к типам. Т.е. поднялись на один уровень
  абстракции выше.

  Сейчам мы сделали в целом то же самое. Написали обычную программу,
  но в качестве аргументов передали ей типы.

  На самом деле, мы уже даже ввели типы для типов. Kind'ы описывают типы
  так же, как типы описывают значения. А раз нам уже понадобилось программировать
  на уровне типов, может ли нам понадобится программирование на уровне kind'ов?

  На самом деле в теории типов эта цепочка продолжается бесконечно, а очередное
  звено этой цепи обозначают TypeN. Т.е. значения в Haskell это Type, типы — Type1,
  kind'ы — Type2 и так далее до бесконечности. Цепочку сделали бесконечной,
  чтобы избежать парадокс Рассела (со множеством всех множеств).

    Type :: Type1 :: Type2 :: Type3 : ...

  Это называется type universes. Язык програмирования Agda реализована такая
  бесконечная цепочка.
  https://agda.readthedocs.io/en/v2.6.0/language/universe-levels.html
-}

{-
  Хотим создать список заданного размера. Как мы можем это сделать?

  Примерное поведение:
  
    Nil :: List a 0
    Cons 2 Nil :: List Integer 1
    Cons 3 $ Cons 2 Nil :: List Integer 2
    ...
  
  Ниже рассмотрим как получить такое поведение.
-}

{-
  Определим тип натуральных чисел, используя арифметику Пеано:

    0 = Zero
    1 = Succ Zero
    2 = Succ $ Succ Zero
    ...
-} 
data Nat = Zero | Succ Nat
  deriving (Eq, Show)

-- Функция для удобства для конвертации
fromInt :: Int -> Nat
fromInt n = case abs n of
  0 -> Zero
  n' -> Succ (fromInt $ n' - 1)

{-
  Пока всё в рамках Haskell98:

  kinds :    Type
              |
  types :    Nat
            /   \
  terms : Zero Succ
-}

{-
  Теперь определим SNat — синглтон.

  Если посмотреть на его определение, то можно заметить, что
  структурно он повторяет собой Nat. У него также есть конструктор
  для 0 — SZero, как у Nat — Zero. И SSucc, как Succ, для выражения (+ 1).

  Отличие в том, что SNat параметризован типом Nat. То есть,
  SNat — это конструктор типов, который на вход принимает типы 'Zero
  и 'Succ, у которых кайнд Nat.
-}
data SNat :: Nat -> Type where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

{-
  Мы получили следующую картину:

  kinds :    Type         Nat (kind)
              |            /    \
  types :    Nat        'Zero 'Succ      SNat ['Zero или 'Succ] (singleton type)
            /   \                            /      \
  terms : Zero Succ                       SZero   SSucc

  По умолчанию, Haskell стирает все типы во время компиляции и в рантайме типы не присутствуют.
  Когда мы промоутим терм Zero до значения 'Zero кайнда Nat, то он существует только на стадии тайпчекинга.

  Синглтон позволяет нам иметь runtime версию типа. Имея терм SZero,
  мы можем вызывать функцию, которая ожидает тип SNat 'Zero, и так далее по индукции.
  
  Заметим, что конструкторы SNat повторяют конструкторы Nat. И для каждого
  типа `SNat n` (то есть полностью примененного, не ожидающего аргумент),
  существует только один терм (кроме undefined):

    Zero <-> SZero
    Succ n <-> SSucc n

  Именно поэтому SNat называется синглтоном. Он повторяет структуру Nat, а
  конструкторы Nat изоморфны типам, которые параметризуют синглтон:
  
    Zero <-> 'Zero
    Succ <-> 'Succ
-}

fromSNat :: SNat n -> Nat
fromSNat SZero = Zero
fromSNat (SSucc n) = Succ $ fromSNat n

{-
  SNat 'Zero, SNat ('Succ 'Zero), SNat ('Succ $ 'Succ 'Zero), ...

  toSNat :: Nat -> SNat n
  toSNat Zero = SZero
  toSNat (Succ n) = SSucc (toSNat n)
-}

-- Existential type — a pattern
data SomeSNat where
  SomeSNat :: SNat n -> SomeSNat

natToSomeSNat :: Nat -> SomeSNat
natToSomeSNat Zero = SomeSNat SZero
natToSomeSNat (Succ n) = case natToSomeSNat n of
  SomeSNat sn -> SomeSNat $ SSucc sn

withSomeNat :: SomeSNat -> (forall (n :: Nat) . SNat n -> result) -> result
withSomeNat (SomeSNat n) f = f n

data Vec a (n :: Nat) where
  VNil :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

instance Show a => Show (Vec a n) where
  show = \case
    VNil -> "nil"
    (VCons x xs) -> show x ++ " : " ++ show xs

data SomeVec a where 
  SomeVec :: Vec a n -> SomeVec a

instance Show a => Show (SomeVec a) where
  show (SomeVec v) = show v

-- аналог функции replicate для списков
replicateVec :: SNat n -> a -> Vec a n
replicateVec sn x = case sn of
  SZero -> VNil
  (SSucc sn) -> VCons x (replicateVec sn x)

askVecWithZeros :: IO ()
askVecWithZeros = do
  n :: Nat <- fromInt . read <$> getLine
  print $ withSomeNat (natToSomeSNat n) $ \sn -> SomeVec $ replicateVec sn 'a'

-- why not SNat n
vlength :: SomeVec a -> Integer
vlength (SomeVec vec) = go 0 vec
  where
    go :: Integer -> Vec a n -> Integer
    go size = \case
      VNil -> size
      (VCons _ xs) -> go (size + 1) xs

{-
  Хотим написать функцию для получения элемента по заданному индексу
-}

data Decision where
  Yes :: Decision
  No :: Decision

type family (:<=) (m :: Nat) (n :: Nat) :: Decision where
  (:<=) 'Zero _ = 'Yes
  (:<=) ('Succ x) 'Zero = 'No
  (:<=) ('Succ x) ('Succ y) = (:<=) x y

getAt
  :: forall a (m :: Nat) (n :: Nat)
  .  (:<=) m n ~ 'Yes
  => SNat m -> Vec a ('Succ n) -> a
getAt SZero (VCons x _) = x
getAt (SSucc m) (VCons _ (VCons x xs)) = getAt m (VCons x xs)

-- <Задачи для самостоятельного решения>

-- Напишите аналог zip для Vec n
vzip :: Vec a n -> Vec b n -> Vec (a, b) n
vzip VNil VNil = VNil
vzip (VCons x xs) (VCons y ys) = VCons (x, y) (vzip xs ys)
-- </Задачи для самостоятельного решения>

{- printf

  Всем нам знакомы функции форматированного вывода. Все они устроены примерно одинаково:

    print(stringWithFormat, arg1, arg2, ...)

  В качестве первого аргумента такие функции принимать строку, в которой используются
  специальные символы для описания формата вывода для конкретного типа, а затем
  значения, которые и нужно вывести на экран.

  В С++ такая функция называется printf http://www.cplusplus.com/reference/cstdio/printf/

    printf ("%s %4.2f %010d \n", "A string", 3.1416, 1977);

  Понятно, что при компиляции невозмозможно проверить, правильно ли были выбраны
  спец.символы, ну или верного ли типа аргументы были переданы.

  Давайте напишем строго типизированный printf, который будет проверять типы
  аргументов, доставая информацию о них из переданного формата.

  Стандарт говорит:
    If any argument is not the correct type for the corresponding
    conversion specification, the behavior is undefined.

  https://stackoverflow.com/questions/38597274/why-does-printff-0-give-undefined-behavior

  Решение задачи с помощью зависимых типов: src/Lecture13.idr
-}

{- Ссылки

  - 'type family' vs 'data family', in brief
    https://stackoverflow.com/a/20908500
  - Implementing a minimal version of haskell-servant
    https://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/
  - Dependently Typed Programming with Singletons
    https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf
  - GHC TypeLits
    https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-TypeLits.html
  - GADTs
    https://downloads.haskell.org/~ghc/6.6/docs/html/users_guide/gadt.html
  - Десериализация с зависимыми типами
    https://ruhaskell.org/posts/theory/2016/01/06/serialization-with-deptypes.html
  - Type-safe dependently-typed printf in Idris
    https://gist.github.com/chrisdone/672efcd784528b7d0b7e17ad9c115292
  - Universe Levels in Agda
    https://agda.readthedocs.io/en/v2.6.0/language/universe-levels.html
-}