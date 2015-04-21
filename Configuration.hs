module Configuration (getConfigOption, configFileExists
                     ,createConfigDirIfMissing)
where

import System.Directory
import Data.Configurator
import Data.Configurator.Types
import Data.Text
import Control.Exception (bracket)

configFilePrefix :: String
configFilePrefix = configDirPrefix ++ "/rc.conf"

configDirPrefix :: String
configDirPrefix = "/.workcalendar"

configFilePosition :: IO String
configFilePosition
    = fmap (++ configFilePrefix) getHomeDirectory

configDirPosition :: IO String
configDirPosition
    = fmap (++ configDirPrefix) getHomeDirectory

configFileExists :: IO Bool
configFileExists = configFilePosition >>= doesFileExist

-- This procedure creates the directory where the configuration files
-- for the calendar go, it returns True if the directory already
-- existed
createConfigDirIfMissing :: IO Bool
createConfigDirIfMissing 
    = configDirPosition >>= 
      \path -> doesDirectoryExist path >>=
      \exists -> (createDirectoryIfMissing True path) >>
      return exists

loadConfig :: IO Config
loadConfig = let actions = \path -> (load [Required path])
                 errorHandler = \_ -> return Data.Configurator.empty
             in bracket configFilePosition actions errorHandler

getConfigOption :: String -> IO (Maybe String)
getConfigOption key = loadConfig >>=
                      \conf -> Data.Configurator.lookup conf (pack key)
