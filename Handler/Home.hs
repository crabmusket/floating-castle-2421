{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Text.Julius (rawJS)

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost messageForm
    defaultLayout $ do
        setTitle "floating-castle"
        $(widgetFile "homepage")
            where formId = "theForm" :: Text

postHomeR :: Handler Value
postHomeR = do
    ((result, _), _) <- runFormPost messageForm

    case result of
        FormSuccess msg -> do
            _ <- runDB $ insert msg
            return $ object ["success" .= True]
        _ -> return $ object ["error" .= ("Invalid submission." :: Text)]

messageForm :: Form Message
messageForm = renderDivs $ Message
    <$> areq textField "Add an item:" Nothing
