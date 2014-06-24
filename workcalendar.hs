import Colors
import Configuration
import Calendar
import ArgumentHandling

-- Color support?
colorSupport :: [Argument] -> Bool
colorSupport args
    = elem (ColorSupport True) args

-- Program logic
getWorkDayTable :: (Day -> Bool) -> CalendarMonad (Weekday,Bool)
getWorkDayTable workDay 
    = CalendarMonad $ \day -> let weekday = formatTimeToWeekday day
                              in (dayNext day, (weekday, workDay day))

regularWorkDays :: IO [Weekday]
regularWorkDays = let weekdayString :: IO String
                      weekdayString = fmap (maybe "" id) $ getConfigOption "default.workdays"
                  in fmap ((map read) . words) weekdayString

uniqueWorkDays :: IO [Day]
uniqueWorkDays 
    = let getString :: IO (Maybe String)
          getString = getConfigOption "unique.workdays"
          unpackMaybeString :: Maybe String -> String
          unpackMaybeString = maybe "" id
          stringToDays :: String -> [Day]
          stringToDays s = map read (words s)
      in fmap (stringToDays . unpackMaybeString) getString
         
         
isWorkDay :: (Weekday -> Bool) -> (Day -> Bool) -> Day -> Bool
isWorkDay regular unique day
    = regular (formatTimeToWeekday day) ||
      (unique day)

listNextDays :: Day -> Int -> (Day -> Bool) -> [(Weekday, Bool)]
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
          workdayToString False = condGreen ++ "free" ++ condNormal
          condRed = if colorsupport then red else ""
          condGreen = if colorsupport then green else ""
          condNormal = if colorsupport then normal else ""
      in unlines $ map makeLine dates

main :: IO ()
main = do
  cmdLineArgs <- getCommandLineArgs
  color <- return (colorSupport cmdLineArgs)
  today <- getToday
  regular <- regularWorkDays
  uniques <- uniqueWorkDays
  putStr $ (\pairs -> makeTable pairs color) $ listNextDays today 5 (isWorkDay (`elem` regular) (`elem` uniques))
