module Main where

import Control.Monad.Trans.Resource
import Requestinator
import Network.HTTP.Client

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let req = undefined
  -- x <- runResourceT $ runRequest manager req
  -- print x
  return ()
