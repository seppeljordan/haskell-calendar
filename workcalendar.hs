import Colors
import Configuration
import Calendar
import ArgumentHandling

-- Color support?
colorSupport :: [Argument] -> Bool
colorSupport args
    = elem (ColorSupport True) args

numberOfDays :: [Argument] -> Int
numberOfDays args
    = let findNmbr :: Argument -> Bool
          findNmbr (NumberOfDays _) = True
          findNmbr _ = False
          filterCorrectArgs = filter findNmbr args
          unwrapNmbr (NumberOfDays n) = n
          unwrapNmbr e = error $ "numberOfDays: unwrapNmbr got wrong input" ++ show e
      in if filterCorrectArgs == []
         then 5
         else (unwrapNmbr $ last (filterCorrectArgs)) 

-- Program logic
getWorkDayTable :: (Day -> Bool) -> CalendarMonad (Weekday,Bool)
getWorkDayTable workDay 
    = CalendarMonad $ \day -> let weekday = formatTimeToWeekday day
                              in (dayNext day, (weekday, workDay day))

regularWorkDays :: IO [Weekday]
regularWorkDays = let weekdayString :: IO String
                      weekdayString = fmap (maybe "" id) $ getConfigOption "default.workdays"
                  in fmap (map read . words) weekdayString

uniqueWorkDays :: IO [Day]
uniqueWorkDays 
    = let getString :: IO (Maybe String)
          getString = getConfigOption "unique.workdays"
          unpackMaybeString :: Maybe String -> String
          unpackMaybeString = maybe "" id
          stringToDays :: String -> [Day]
          stringToDays s = map read (words s)
      in fmap (stringToDays . unpackMaybeString) getString
         
         
isWorkDay :: [Weekday] -> [Day] -> Day -> Bool
isWorkDay regular unique day
    = elem (formatTimeToWeekday day) regular ||
      (elem day unique)

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
main =
    getCommandLineArgs >>= \cmdLineArgs -> 
        let color = colorSupport cmdLineArgs
            howManyDays = numberOfDays cmdLineArgs
        in getToday >>= \today -> 
            regularWorkDays >>= \regular -> 
                uniqueWorkDays >>= \uniques ->
                let renderTable :: [(Weekday, Bool)] -> String
                    renderTable pairs = makeTable pairs color
                    generateInformation = listNextDays today howManyDays
                                          (isWorkDay regular uniques)
                in putStr $ renderTable $ generateInformation
