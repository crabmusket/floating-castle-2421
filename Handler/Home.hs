{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Text.Julius (rawJS)

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let formId = "theForm" :: Text
    defaultLayout $ do
        setTitle "floating-castle"
        $(widgetFile "homepage")

postHomeR :: Handler Value
postHomeR = do
    ((result, _), _) <- runFormPost sampleForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    jsonOrRedirect HomeR (object $ ["value" .= show submission])

sampleForm :: Form Text
sampleForm = renderDivs $ areq textField "Add an item:" Nothing
