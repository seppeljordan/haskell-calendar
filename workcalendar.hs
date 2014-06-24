import Control.Applicative
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

import Colors
import Configuration
import Calendar
import ArgumentHandling

-- Color support?
colorSupport :: [Argument] -> Bool
colorSupport args
    = elem (ColorSupport True) args

-- Program logic
getWorkDayTable :: (Weekday -> Bool) -> CalendarMonad (Weekday,Bool)
getWorkDayTable workDay 
    = CalendarMonad $ \day -> let weekday = formatTimeToWeekday day
                              in (dayNext day, (weekday, workDay weekday))

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

makeTable :: [(Weekday, Bool)] -> Bool -> String
makeTable dates colorsupport
    = let makeLine :: (Weekday, Bool) -> String
          makeLine (wd,bool) = "| " ++ show wd ++ "\t | " ++ (workdayToString bool) ++ " |"
          workdayToString :: Bool -> String
          workdayToString True  = condRed ++ "work" ++ condNormal
          workdayToString False = "free"
          condRed = if colorsupport then red else ""
          condGreen = if colorsupport then green else ""
          condNormal = if colorsupport then normal else ""
      in unlines $ map makeLine dates

main :: IO ()
main = do
  cmdLineArgs <- getCommandLineArgs
  color <- return (colorSupport cmdLineArgs)
  today <- getToday
  workdays <- workDayList
  putStr $ (\pairs -> makeTable pairs color) $ listNextDays today 5 (\x -> elem x workdays)
