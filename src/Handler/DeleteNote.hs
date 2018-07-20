{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.DeleteNote where

import Import

getDeleteNoteR :: NoteId -> Handler Html
getDeleteNoteR noteId = do
    isAjax <- isAjaxRequest
    let confirmWidget = $(widgetFile "notes/confirm/delete")
    if isAjax then ajaxContentLayout confirmWidget
    else do
        note <- runDB $ get404 noteId
        defaultLayout $ do
            let noteWidget = $(widgetFile "notes/view")
            setTitle "Delete note"
            $(widgetFile "notes/confirm/withNote")

postDeleteNoteR :: NoteId -> Handler TypedContent
postDeleteNoteR noteId = do
    runDB $ delete noteId
    setMessage "Your note was successfully deleted"
    isAjax <- isAjaxRequest
    if isAjax then selectRep $ provideRep $ ajaxContentLayout $ toWidget [hamlet|<h1>Deleted|]
    else selectRep $ do
        provideRep (redirect NotesR :: Handler Html)
        provideJson ("Note deleted successfully" :: Text)