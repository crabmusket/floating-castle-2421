module Handler.Messages where

import Import hiding (parseTime)
import Text.Julius (rawJS)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)
import Data.Text (unpack)

getMessagesR :: Handler Value
getMessagesR = do
    timeString <- runInputGet $ ireq textField "time"
    let time = parseUTCTime timeString
    case time of
        Nothing -> return $ object ["messages" .= False]
        Just t  -> do
            messages <- runDB $ selectList [MessagePosted >. t] [Desc MessagePosted]
            let jsonMessages = map jsonMessage messages
            return $ object ["messages" .= jsonMessages]

postMessagesR :: Handler Value
postMessagesR = do
    ((result, _), _) <- runFormPost messageForm
    case result of
        FormSuccess msg -> do
            _ <- runDB $ insert msg
            return $ object ["success" .= True]
        _ -> return $ object ["error" .= ("Invalid submission." :: Text)]

deleteMessagesR :: Handler Value
deleteMessagesR = error "Not yet implemented: deleteMessagesR"

messageForm :: Form Message
messageForm = renderDivs $ Message
    <$> areq textField "Add an item:" Nothing
    <*> lift (liftIO getCurrentTime)

parseUTCTime :: Text -> Maybe UTCTime
parseUTCTime = parseTime defaultTimeLocale "%F %k:%M:%S%Q" . unpack

jsonMessage :: Entity Message -> Value
jsonMessage (Entity mid m) = object
    [ "text" .= messageText m
    , "posted" .= messagePosted m
    , "id" .= show mid
    ]
