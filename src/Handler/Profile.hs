{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Profile where

import Import
import qualified Data.Map as Map

getProfileR :: Handler TypedContent
getProfileR = do
    (uid, user) <- requireAuthPair
    noteNumber <- runDB $ count [NoteUserId ==. uid]
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle . toHtml $ userIdent user <> "'s User page"
            $(widgetFile "profile")
        provideJson $ Map.fromList [
                ("username" :: Text, userIdent user),
                ("note_number", pack $ show noteNumber)
            ]