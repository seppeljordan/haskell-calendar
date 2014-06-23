module Configuration (getConfigOption, configFileExists
                     ,createConfigDirIfMissing)
where

import System.Directory
import Data.Configurator
import Data.Configurator.Types
import Control.Monad.Except
import Data.Text

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
loadConfig = let actions = configFilePosition >>=
                           \path -> (load [Required path])
                 errorHandler = \_ -> return Data.Configurator.empty
             in catchError actions errorHandler

getConfigOption :: String -> IO (Maybe String)
getConfigOption key = loadConfig >>=
                      \conf -> Data.Configurator.lookup conf (pack key)
