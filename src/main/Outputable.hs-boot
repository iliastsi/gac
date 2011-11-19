module Outputable where

progName :: String

panic :: String -> a

printErrs   :: [String] -> IO ()
printWarns  :: [String] -> IO ()
printOutput :: [String] -> IO ()
