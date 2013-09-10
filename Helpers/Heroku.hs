{-# LANGUAGE OverloadedStrings #-}

module Helpers.Heroku (herokuConf) where

import Prelude
import Data.Text (Text, pack, unpack, breakOn)
import Database.Persist.MongoDB (MongoConf(..), master, MongoAuth(..))
import Network (PortID(PortNumber))
import Data.Word (Word16)
import Network.URI
import System.Environment

import qualified Data.Text as T

herokuConf :: IO MongoConf
herokuConf = do
    params <- dbConnParams
    let
        getParam n = case lookup n params of
            Just v -> v
            Nothing -> error $ "Could not find parameter " ++ unpack n ++ " in database config URL."
        dbname = getParam "dbname"
        host = getParam "host"
        port = PortNumber $ fromIntegral (read $ T.unpack (getParam "port") :: Word16)
        username = getParam "user"
        password = getParam "password"
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
dbConnParams = getEnv "MONGOLAB_URI" >>= return . parseDatabaseUrl

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
     in
        -- Prelude.tail is unsafe but these settings should always exist.
         [(pack "user",     user)
         ,(pack "password", T.tail password)
         ,(pack "host",     pack $ uriRegName auth)
         ,(pack "port",     pack $ Prelude.tail $ uriPort auth)
         ,(pack "dbname",   pack $ Prelude.tail $ dbpath)
         ]
  where
    -- Prelude.init is not safe, but should be there on Heroku
    userAndPassword :: URIAuth -> (Text, Text)
    userAndPassword = (breakOn $ pack ":") . pack . Prelude.init . uriUserInfo
    schemeError uri = error $ "was expecting a mongodb scheme, not: " ++ (uriScheme uri) ++ "\n" ++ (show uri)
    invalid = error "could not parse heroku MONGOLAB_URI"

