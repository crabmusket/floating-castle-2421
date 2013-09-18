{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Text.Julius (rawJS)
import Handler.Messages (messageForm, showMarkdown, showUTCTime)

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost messageForm
    messages <- runDB $ selectList [] [Desc MessagePosted]
    msgRender <- getMessageRender
    formId <- newIdent
    listId <- newIdent
    let jsLocal = rawJS . msgRender
        messagesWidget = $(widgetFile "messages")
    defaultLayout $ do
        setTitle "floating-castle"
        $(widgetFile "homepage")
