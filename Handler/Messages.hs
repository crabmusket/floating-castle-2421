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

getMessagesR :: Handler Html
getMessagesR = do
    timeString <- runInputGet $ ireq textField "time"
    case parseUTCTime timeString of
        Nothing -> invalidArgs ["Misunderstood time format"] >> return ""
        Just t -> do
            messages <- runDB $ selectList [MessagePosted >. t] [Asc MessagePosted]
            listId <- newIdent
            pc <- widgetToPageContent $(widgetFile "messages")
            giveUrlRenderer [hamlet| ^{pageBody pc} |]

postMessagesR :: Handler Html
postMessagesR = do
    ((result, _), _) <- runFormPost messageForm
    case result of
        FormSuccess msg -> do
            messageId <- runDB $ insert msg
            let messages = [Entity messageId msg]
            pc <- widgetToPageContent $(widgetFile "messages")
            giveUrlRenderer [hamlet| ^{pageBody pc} |]
        _ -> invalidArgs ["Invalid input"] >> return ""

deleteMessagesR :: Handler Html
deleteMessagesR = do
    idString <- runInputPost $ ireq textField "id"
    case (fromPathPiece idString :: Maybe MessageId) of
        Nothing -> invalidArgs ["Invalid input"] >> return ""
        Just mid -> do
            runDB $ delete mid
            return [shamlet| #{toPathPiece mid} |]

putMessagesR :: Handler Html
putMessagesR = do
    (idString, newText) <- runInputPost $ (,)
        <$> ireq textField "id"
        <*> ireq textField "text"
    case fromPathPiece idString of
        Nothing -> invalidArgs ["Invalid message ID"] >> return ""
        Just messageId -> do
            newMessage <- runDB $ do
                update messageId [MessageText =. newText]
                get messageId
            case newMessage of
                Nothing -> notFound >> return ""
                Just msg -> do
                    let messages = [Entity messageId msg]
                    pc <- widgetToPageContent $(widgetFile "messages")
                    giveUrlRenderer [hamlet| ^{pageBody pc} |]

messageForm :: Form Message
messageForm = renderDivs $ Message
    <$> areq textField "" Nothing
    <*> lift (liftIO getCurrentTime)

parseUTCTime :: Text -> Maybe UTCTime
parseUTCTime = parseTime defaultTimeLocale "%F %T%Q" . unpack

showUTCTime :: UTCTime -> Text
showUTCTime = pack . formatTime defaultTimeLocale "%F %TZ"

showMarkdown :: Text -> Html
showMarkdown = markdown def . fromStrict

renderMarkdown :: Text -> Text
renderMarkdown = toStrict . renderHtml . showMarkdown
