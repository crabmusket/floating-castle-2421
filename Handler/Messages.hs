{-# LANGUAGE TupleSections #-}
module Handler.Messages where

import Import hiding (parseTime)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (defaultTimeLocale)
import Data.Text (pack, unpack)
import Data.Text.Lazy (fromStrict, toStrict)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown (markdown, def)
import Data.Maybe (fromJust)

getMessagesR :: Handler Value
getMessagesR = do
    timeString <- runInputGet $ ireq textField "time"
    case parseUTCTime timeString of
        Nothing -> return $ object ["messages" .= False]
        Just t  -> do
            messages <- runDB $ selectList [MessagePosted >. t] [Asc MessagePosted]
            let jsonMessages = map jsonMessage messages
            return $ object ["messages" .= jsonMessages]

postMessagesR :: Handler Value
postMessagesR = do
    ((result, _), _) <- runFormPost messageForm
    case result of
        FormSuccess msg -> do
            mid <- runDB $ insert msg
            return $ object ["message" .= jsonMessage (Entity mid msg)]
        _ -> return $ object ["error" .= ("Invalid submission." :: Text)]

deleteMessagesR :: Handler Value
deleteMessagesR = do
    idString <- runInputPost $ ireq textField "id"
    case (fromPathPiece idString :: Maybe MessageId) of
        Nothing -> return $ object []
        Just mid -> do
            runDB $ delete mid
            return $ object ["deleted" .= idString]

putMessagesR :: Handler Value
putMessagesR = do
    (idString, newText) <- runInputPost $ (,)
        <$> ireq textField "id"
        <*> ireq textField "text"
    case fromPathPiece idString of
        Nothing -> return $ object []
        Just mid -> do
            newMessage <- runDB $ do
                update mid [MessageText =. newText]
                get mid
            return $ object ["message" .= jsonMessage (Entity mid $ fromJust newMessage)]

messageForm :: Form Message
messageForm = renderDivs $ Message
    <$> areq textField "" Nothing
    <*> lift (liftIO getCurrentTime)

parseUTCTime :: Text -> Maybe UTCTime
parseUTCTime = parseTime defaultTimeLocale "%F %T%Q" . unpack

showUTCTime :: UTCTime -> Text
showUTCTime = pack . formatTime defaultTimeLocale "%F %TZ"

jsonMessage :: Entity Message -> Value
jsonMessage (Entity mid m) = object
    [ "text" .= renderMarkdown (messageText m)
    , "raw" .= messageText m
    , "posted" .= showUTCTime (messagePosted m)
    , "id" .= toPathPiece mid
    ]

showMarkdown :: Text -> Html
showMarkdown = markdown def . fromStrict

renderMarkdown :: Text -> Text
renderMarkdown = toStrict . renderHtml . showMarkdown
