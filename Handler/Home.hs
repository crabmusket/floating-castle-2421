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
    let jsLocal = rawJS . msgRender
    defaultLayout $ do
        setTitle "floating-castle"
        $(widgetFile "homepage")
            where formId = "theForm" :: Text
                  listId = "theList" :: Text
