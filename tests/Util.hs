module Util (assertNothing) where

import Test.Tasty.HUnit (assertFailure)

assertNothing :: String -> Maybe a -> IO ()
assertNothing _ Nothing = return ()
assertNothing m (Just _) = assertFailure m
