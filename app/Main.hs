module Main where

import Control.Monad.Trans.Resource
import Requestinator
import Network.HTTP.Client
import Jikan

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let req = GetAnime 1
  x <- runResourceT $ runRequest manager req
  print x
  return ()
