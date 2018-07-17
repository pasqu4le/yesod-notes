{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    (uid, user) <- requireAuthPair
    noteNumber <- runDB $ count [NoteUserId ==. uid]
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")