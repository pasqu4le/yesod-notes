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
            setTitle "Profile Page"
            $(widgetFile "profile")
        provideJson $ Map.fromList [
                ("user_identifier" :: Text, userIdent user),
                ("note_number", pack $ show noteNumber)
            ]