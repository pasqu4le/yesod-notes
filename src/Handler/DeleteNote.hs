{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.DeleteNote where

import Import

getDeleteNoteR :: NoteId -> Handler Html
getDeleteNoteR noteId = do
    isAjax <- isAjaxRequest
    let confirmWidget = $(widgetFile "notes/confirmDelete")
    if isAjax then ajaxContentLayout confirmWidget
    else do
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
    isAjax <- isAjaxRequest
    if isAjax then ajaxContentLayout $ toWidget [hamlet|<h1>Deleted|]
    else redirect NotesR