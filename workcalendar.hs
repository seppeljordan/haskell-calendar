import Control.Applicative
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

import Configuration

getWorkDayTable :: (Weekday -> Bool) -> CalendarMonad (Weekday,Bool)
getWorkDayTable workDay = CalendarMonad $ \day -> (addDay 1 day, (day, workDay day))

workDayList :: IO [Weekday]
workDayList = let weekdayString :: IO String
                  weekdayString = fmap (maybe "" id) $ getConfigOption "default.workdays"
              in fmap ((map read) . words) weekdayString
         

listNextDays :: Day -> Int -> (Weekday -> Bool) -> [(Weekday, Bool)]
listNextDays startDate n procedure = let nd 0 accu = return accu
                                         nd m accu = do
                                           wd <- getWorkDayTable procedure 
                                           nd (m-1) (wd:accu)
                                         CalendarMonad runIt = nd n []
                                     in reverse $ snd $ (runIt startDate)

makeTable :: [(Weekday, Bool)] -> String
makeTable dates = let makeLine :: (Weekday, Bool) -> String
                      makeLine (wd,bool) = "| " ++ show wd ++ "\t | " ++ (workdayToString bool) ++ " |"
                      workdayToString :: Bool -> String
                      workdayToString True  = "Arbeit"
                      workdayToString False = "  frei"
                  in unlines $ map makeLine dates


getCurrentTime :: IO LocalTime
getCurrentTime = fmap zonedTimeToLocalTime getZonedTime

main :: IO ()
main = do
  wd <- fmap formatTimeToWeekday getCurrentTime
  workdays <- workDayList
  putStr $ makeTable $ listNextDays wd 5 (\x -> elem x workdays)
