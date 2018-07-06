{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Note where

import Import

getNoteR :: NoteId -> Handler TypedContent
getNoteR noteId = do
    note <- runDB $ get404 noteId
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Note"
            $(widgetFile "notes/view")
        provideJson note
