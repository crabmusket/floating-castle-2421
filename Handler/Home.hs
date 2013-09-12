{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Text.Julius (rawJS)
import Text.Markdown (markdown, def)
import Data.Text.Lazy (fromStrict)
import Handler.Messages (messageForm)

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost messageForm
    messages <- runDB $ selectList [] [Desc MessagePosted]
    defaultLayout $ do
        setTitle "floating-castle"
        $(widgetFile "homepage")
            where formId = "theForm" :: Text
                  listId = "theList" :: Text

showMarkdown :: Text -> Html
showMarkdown = markdown def . fromStrict
