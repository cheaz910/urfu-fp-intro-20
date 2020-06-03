module Server where

import Control.Monad.Trans.Except
import Control.Monad.Catch as C
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT)
import Servant as S

import App
import API
import Handlers (getSessions, getSeats, postPreliminary, checkout, refund)
import Utils

{-
  Для сервера мы используем библиотеку servant-server. Которая предоставляет нам
  трансформер `ServerT`, где первым параметром идет тип, описывающий наше API. В нашем случае
  это `BookMovieAPI`, определенный в `API.hs`. Вторым параметром идет уже наш трансформер
  для приложения `AppT` из модуля `App`.

  Сам сервер — это набор хэндлеров, соединенных с помощью оператора `:<|>`.
-}
bookingServer :: MonadIO m => ServerT BookMovieAPI (AppT m)
bookingServer = (getSessions :<|> getSeats)
  :<|> postPreliminary
  :<|> checkout
  :<|> refund

{-
  Функция, которая создает servant приложение `Application`.
-}
mkApplication :: Config -> Application
mkApplication config = serve bookMovieAPI mkServer
  where
    mkServer = hoistServer bookMovieAPI (convertApp config) bookingServer

{-
  Эта функция конвертирует наше вычисление, завернутое в трансформер `AppT`,
  в серверный хэндлер библиотеки servant `S.Handler`.

  Конвертируем мы его, запуская наш трансформер с помощью `runApp app`, а потом
  `runReaderT`, передавая конфигурацию приложения. Функция `catchErrors` уже запускает
  `ExceptT`, и мы навешиваем обработку исключений, чтобы выводить сообщения об ошибках
  в stdout.

  Мы можем расширить `errHandler` до более серьезного логгирования например при помощи библиотеки
  `katip`, которая позволит нам складывать логи в Elasticsearch для удобного поиска.
-}
convertApp :: Config -> AppT IO a -> S.Handler a
convertApp config app = catchErrors $ runReaderT (runApp app) config
  where
    errHandler :: C.Handler IO (Either ServerError a)
    errHandler = C.Handler $ \(SomeException e) -> do
      print e
      pure $ Left $ toServerError $ displayException e
    catchErrors :: ExceptT ServerError IO a -> S.Handler a
    catchErrors = S.Handler .
      ExceptT .
      flip catches [errHandler] .
      runExceptT
