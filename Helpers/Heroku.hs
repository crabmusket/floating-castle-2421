{-# LANGUAGE OverloadedStrings #-}

module Helpers.Heroku (herokuConf) where

import Prelude
import Database.Persist.MongoDB (MongoConf(..), master, MongoAuth(..))
import Web.Heroku (dbConnParams)
import Data.Maybe (fromJust)
import Network (PortID(PortNumber))
import Network.Socket (PortNumber(PortNum))
import Data.Word (Word16)

import qualified Data.Text as T

herokuConf :: IO MongoConf
herokuConf = do
    params <- dbConnParams
    let
        dbname = fromJust $ lookup "dbname" params
        host = fromJust $ lookup "host" params
        port = PortNumber $ PortNum (read $ T.unpack (fromJust $ lookup "port" params) :: Word16)
        username = fromJust $ lookup "user" params
        password = fromJust $ lookup "password" params
     in return MongoConf {
        mgDatabase = dbname,
        mgHost = host,
        mgPort = port,
        mgAuth = Just $ MongoAuth username password,
        mgAccessMode = master,
        mgPoolStripes = 10,
        mgStripeConnections = 1,
        mgConnectionIdleTime = 1
     }

