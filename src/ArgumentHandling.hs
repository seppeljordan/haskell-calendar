module ArgumentHandling (Argument (..), getCommandLineArgs)
where

import System.Console.GetOpt
import System.Environment

-- Command line options
data Argument = ColorSupport Bool | NumberOfDays Int
                deriving (Show, Eq)

usage :: String
usage =
    let usageHeader = "A small calendar application for displaying work days\n" ++
                      "\n" ++
                      "Specify your work days in ~/.workcalendar/rc.conf\n"
    in usageInfo usageHeader options

options :: [OptDescr Argument]
options
    = [ Option ['c'] ["color"] (ReqArg parseColorSetting "MODE") "Color mode settings: always, never"
      , Option ['n'] ["next-days"] (ReqArg (\x -> NumberOfDays (read x)) "INTEGER") "List upcomming INTEGER days"
      ]

parseColorSetting :: String -> Argument
parseColorSetting "always" = ColorSupport True
parseColorSetting "never" = ColorSupport False
parseColorSetting _ = error $ "Wrong color setting\n" ++ usage

checkArgErrors :: ([Argument], [String], [String]) -> ([Argument], [String])
checkArgErrors (_, _, err:_) = error err
checkArgErrors (args, unuseds, _) = (args, unuseds)

checkUnused :: ([Argument], [String]) -> [Argument]
checkUnused (_, unused:_) = error $ usage ++ "Unknown Argument: " ++ unused ++ "\n"
checkUnused (args, _) = args

getCommandLineArgs :: IO [Argument]
getCommandLineArgs 
    = let check = checkUnused . checkArgErrors . 
                  (getOpt Permute options)
      in fmap check getArgs 
