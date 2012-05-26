{------------------------------------------------------------------------------
    Error.hs
    
    Description: This module contatin errors

    Contact: t: @onlyshk
             m: <"kuleshovmail@gmail.com">    
------------------------------------------------------------------------------}

module Tools.Error where

errorFileNonExistFile :: String
errorFileNonExistFile = "This file is not exist"

errorFileNonExistFile2 :: String -> String
errorFileNonExistFile2 fileName = "This file is not exist " ++ fileName