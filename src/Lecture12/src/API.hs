module API
  ( BookMovieAPI
  , bookMovieAPI
  , module API.Session
  , module API.Preliminary
  , module API.Booking
  -- ^ экспортируем модули, чтобы их содержимое было доступно вместе с модулем `API`
  ) where

import Servant
import API.Session
import API.Preliminary
import API.Booking

{-
  API тип всего приложения, который описывает API методы.
  API методы, как и серверные хэндлеры, комбинируются с помощью типа `:<|>`.

    > : i (:<|>)
    data (:<|>) a b = a :<|> b
-}
type BookMovieAPI
  = MovieSessionsAPI
  :<|> PreliminaryAPI
  :<|> BookingAPI

{-
  Proxy — это особенный тип, который не содержит данных.
  Он определен следующим образом в Data`.Proxy`:

    `data Proxy t = Proxy`

  Это позволяет создавать любой тип `Proxy t` с помощью конструктора `Proxy`.
  Это безопасная замена `undefined :: t`, потому что нет ошибок в рантайме.

  Сам тип бывает полезен, чтобы передать информацию о типах, необходимую только
  во время компиляции (данных в рантайме никаких нет).

  Servant использует очень много type-level конструкций и использование Proxy-типов — один из них.
  Это позволяет нам сообщать servant, что мы строим функции именно для нашего API: `BookMovieAPI`.
-}
bookMovieAPI :: Proxy BookMovieAPI
bookMovieAPI = Proxy