{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.DeleteNote where

import Import

getDeleteNoteR :: NoteId -> Handler Html
getDeleteNoteR noteId = do
    note <- runDB $ get404 noteId
    defaultLayout $ do
        let noteWidget = $(widgetFile "notes/view")
            noNoteButtons = True
        setTitle "Delete note"
        $(widgetFile "notes/delete")

postDeleteNoteR :: NoteId -> Handler Html
postDeleteNoteR noteId = do
    runDB $ delete noteId
    setMessage "Your note was successfully deleted"
    redirect NotesR
