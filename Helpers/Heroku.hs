{-# LANGUAGE OverloadedStrings #-}

module Helpers.Heroku (herokuConf) where

import Prelude
import Data.Text (Text, pack, breakOn)
import Database.Persist.MongoDB (MongoConf(..), master, MongoAuth(..))
import Data.Maybe (fromJust)
import Network (PortID(PortNumber))
import Network.Socket (PortNumber(PortNum))
import Data.Word (Word16)
import Network.URI
import System.Environment

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

dbConnParams :: IO [(Text, Text)]
dbConnParams = getEnv "MONGOHQ_URL" >>= return . parseDatabaseUrl

parseDatabaseUrl :: String -> [(Text, Text)]
parseDatabaseUrl durl =
  let muri = parseAbsoluteURI durl
      (auth, dbpath) = case muri of
                      Nothing ->  error "couldn't parse absolute uri"
                      Just uri -> if uriScheme uri /= "mongodb:"
                                    then schemeError uri
                                    else case uriAuthority uri of
                                           Nothing   -> invalid
                                           Just a -> (a, uriPath uri)
      (user,password) = userAndPassword auth
  in     [
          (pack "user",     user)
          -- tail not safe, but should be there on Heroku
         ,(pack "password", T.tail password)
         ,(pack "host",     pack $ uriRegName auth)
         -- Heroku should use default port
         -- ,(pack "port",     pack $ uriPort auth)
         -- tail not safe but path should always be there
         ,(pack "dbname",   pack $ Prelude.tail $ dbpath)
         ]
  where
    -- init is not safe, but should be there on Heroku
    userAndPassword :: URIAuth -> (Text, Text)
    userAndPassword = (breakOn $ pack ":") . pack . Prelude.init . uriUserInfo

    schemeError uri = error $ "was expecting a postgres scheme, not: " ++ (uriScheme uri) ++ "\n" ++ (show uri)
    -- should be an error 
    invalid = error "could not parse heroku DATABASE_URL"

