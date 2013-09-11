module Handler.Messages where

import Import
import Data.Time (getCurrentTime)

getMessagesR :: Handler Value
getMessagesR = do
    error "Not yet implemented: deleteMessagesR"

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
