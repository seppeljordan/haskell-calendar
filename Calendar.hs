module Calendar (Weekday (..), Day, CalendarMonad (..), 
                 formatTimeToWeekday, getToday, 
                 getCurrentWeekday, dayNext)
                
where

import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import Control.Applicative
import System.Locale

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
               deriving (Show, Eq, Enum, Read)

dayNext :: Day -> Day
dayNext day = addDays 1 day

formatTimeToWeekday :: FormatTime a => a -> Weekday
formatTimeToWeekday ft
    = let formatString = "%w"
          stringToWd s | s == "0" = Sunday
                       | s == "1" = Monday
                       | s == "2" = Tuesday
                       | s == "3" = Wednesday
                       | s == "4" = Thursday
                       | s == "5" = Friday
                       | s == "6" = Saturday
                       | otherwise = error $ 
                                     "stringToWd: only 0 - 6 are valid weekday codes, given was " ++ s
      in stringToWd $ formatTime defaultTimeLocale formatString ft

-- m a = m (s -> (s,a))
newtype CalendarMonad a = CalendarMonad (Day -> (Day, a))

instance Functor CalendarMonad where
    fmap f (CalendarMonad transform)
        = CalendarMonad $ \state0 -> let (state1, value0) = transform state0
                                         value1 = f value0
                                     in (state1, value1)

instance Applicative CalendarMonad where
    pure a = CalendarMonad (\state -> (state, a))
    CalendarMonad transform0 <*> CalendarMonad transform1
        = CalendarMonad $ \state0 -> let (state1, f) = transform0 state0
                                         (state2, value1) = transform1 state1
                                         value2 = f value1
                                     in (state2, value2)

instance Monad CalendarMonad where
    return a = CalendarMonad $ \x -> (x,a)
    (CalendarMonad transform0) >>= ftransform
        = CalendarMonad $ \state0 -> let (state1, value1) = transform0 state0
                                         CalendarMonad transform1 = ftransform value1
                                         (state2, value2) = transform1 state1
                                     in (state2, value2)

getToday :: IO Day
getToday = fmap utctDay getCurrentTime

getCurrentWeekday :: IO Weekday
getCurrentWeekday = fmap formatTimeToWeekday getToday
