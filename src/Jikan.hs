{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Jikan
  ( Anime (..)
  , GetAnime (..)
  , GetAnimeRes (..)
  ) where


import           Data.Aeson
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import           Network.HTTP.Simple
import           Requestinator

data Anime = Anime { title :: String
                   , episodes :: Int
                   } deriving (Generic, Show)

instance FromJSON Anime where
  parseJSON (Object obj) = Anime <$> obj .: "title"
                                 <*> obj .: "episodes"

data GetAnime = GetAnime Int deriving (Show)

data GetAnimeRes = GetAnimeRes Anime deriving (Show)

instance Req GetAnime where
  type Res GetAnime = GetAnimeRes

  request (GetAnime animeId) =
    RQ
    $ setRequestHost "jikan.me"
    $ setRequestPath ("/api/anime/" <> T.encodeUtf8 (T.pack $ show animeId))
    defaultRequest

  response rq = receiveJSON rq GetAnimeRes
  
  
