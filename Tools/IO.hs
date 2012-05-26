{------------------------------------------------------------------------------
    Tools.IO.hs
    
    Description: IO operations

    Contact: t: @onlyshk
             m: <"kuleshovmail@gmail.com">    
------------------------------------------------------------------------------}

module Tools.IO where

import System.Directory

--
-- Read file
-- @filename :: String - file path
-- 
getJsonData :: String -> IO String
getJsonData filename = readFile filename

--
-- Check file exist or not
--
checkFile :: String -> IO Bool
checkFile = doesFileExist