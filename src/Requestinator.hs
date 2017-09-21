{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Requestinator
    ( Req (..)
    , Res (..)
    , HttpRequest (..)
    , runRequest
    , receiveJSON
    , receiveBody
    , receiveEmpty
    , receiveStream
    ) where

import           Conduit
import           Control.Exception
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit.Binary
import           Data.Proxy
import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Conduit hiding (Proxy)
import           Network.HTTP.Types.Status


type ClientResponse = Response (ResumableSource (ResourceT IO) BS.ByteString)

class Req a where
  type Res a :: *
  request :: a -> HttpRequest a
  response :: MonadResource m
           => Request
           -> Proxy a
           -> ClientResponse 
           -> m (Res a)

data HttpRequest a = RQ Request

receiveJSON :: (MonadResource m, FromJSON b)
            => Request
            -> (b -> Res a)
            -> Proxy a
            -> ClientResponse
            -> m (Res a)
receiveJSON req f _ rs = do
  let x = responseBody rs
  body <- liftResourceT (x $$+- sinkLbs)
  case eitherDecode' body of
    Right r -> pure $ f r
    Left e -> throw $ JsonParseException req body e

receiveBody :: (MonadResource m)
            => Request
            -> (BL.ByteString -> Res a)
            -> Proxy a
            -> ClientResponse
            -> m (Res a)
receiveBody _ f _ rs = do
  let x = responseBody rs
  b <- liftResourceT (x $$+- sinkLbs)
  pure $ f b

receiveStream :: (MonadResource m)
              => Request
              -> (ResumableSource (ResourceT IO) BS.ByteString -> Res a)
              -> Proxy a
              -> ClientResponse
              -> m (Res a)
receiveStream _ f _ rs = pure $ f (responseBody rs)


receiveEmpty :: (MonadResource m)
             => Request
             -> Res a
             -> Proxy a
             -> ClientResponse
             -> m (Res a)
receiveEmpty _ f _ _ = pure f

data JsonParseException = JsonParseException Request BL.ByteString String deriving (Show)

instance Exception JsonParseException

data HTTPException = HTTPException Request BL.ByteString  deriving (Show)

instance Exception HTTPException

runRequest :: (MonadResource m, Req req)
           => Manager
           -> req
           -> m (Res req)
runRequest manager rq = do
  let z@(RQ req) = request rq
  rs <- liftResourceT (http req manager)
  let (Status sCode _) = responseStatus rs
  if 200 <= sCode && sCode < 300
    then response req (p z) rs
    else do
    b <- liftResourceT (responseBody rs $$+- sinkLbs)
    throwM $ HTTPException req b
  where 
    p :: HttpRequest a -> Proxy a
    p = const Proxy



