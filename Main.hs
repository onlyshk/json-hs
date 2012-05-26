{------------------------------------------------------------------------------
    Main.hs
    
    Description: Main module

    Contact: t: @onlyshk
             m: <"kuleshovmail@gmail.com">    
------------------------------------------------------------------------------}

module Main where

import System.Environment

import Tools.IO
import Tools.Error
import Tools.Version

import Tools.Json.Data
import Tools.Json.Parser
import Tools.Json.JsonBuilder

main :: IO ()
main = getArgs >>= parseCommandArgs 

parseCommandArgs :: [String] -> IO ()
parseCommandArgs ["--usage"] = usage

parseCommandArgs ["-f", filename] = do 
	checkF <- checkFile filename
	case checkF of
		True -> do  json_data <- getJsonData filename
		            putStrLn json_data
		            return ()

		_ -> putStrLn $ errorFileNonExistFile2 filename

parseCommandArgs _ = putStrLn "try ./json-hs --usage"

usage :: IO ()
usage = putStrLn $ "Usage of json-hs: \n" ++
                   "Parse json and output to stdout: ./json-hs -f file.json"
