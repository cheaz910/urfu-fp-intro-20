{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where

import Control.Monad.IO.Class (MonadIO)
import Servant.Server
import App
import DB.MovieSession
import DB.Seat
import DB.Preliminary
import DB.Booking
import Utils

getSessions :: MonadIO m => AppT m [MovieSession]
getSessions = getMovieSessions

getSeats :: MonadIO m => MovieSessionId -> AppT m [Seat]
getSeats = getSeatsBySessionId

postPreliminary :: MonadIO m => MovieSessionId -> SeatId -> AppT m BookingId
postPreliminary msId seatId = do
  bookings <- createPreliminary msId seatId
  case bookings of
    (b:_) -> pure $ bookingId b
    _ -> throwJSONError err404 $ JSONError "booking is not found"

checkout :: MonadIO m => BookingId -> AppT m String
checkout bId = do
  bookings <- getBookings bId
  if (length bookings == 0) 
    then 
      return "booking is not found"
    else do
      let booking = bookings !! 0
      tryBook booking >>= \case
        Nothing -> return $ "booking was paid successfully"
        Just errorr -> throwJSONError err400 $ errorr

refund :: MonadIO m => BookingId -> AppT m String
refund bId = do
  bookings <- getBookings bId
  case bookings of
    [] -> throwJSONError err404 $ JSONError $ "booking is not found"
    Booking foundBookingId _ _ _ _ : _ -> do
      deleteBooking foundBookingId
      return $ "booking was successfully canceled"