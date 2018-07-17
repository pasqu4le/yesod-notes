{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.EditNote where

import Import
import qualified Form.Note as NoteForm

getEditNoteR :: NoteId -> Handler Html
getEditNoteR noteId = do
    note <- runDB $ get404 noteId
    (formWidget, formEnctype) <- generateFormPost $ NoteForm.fromNote note
    let editNoteWidget = $(widgetFile "notes/editWidget")
    isAjax <- isAjaxRequest
    if isAjax then ajaxContentLayout editNoteWidget
    else defaultLayout $ do
        setTitle "Edit note"
        $(widgetFile "notes/edit")

postEditNoteR :: NoteId -> Handler TypedContent
postEditNoteR noteId = do
    note <- runDB $ get404 noteId
    isAjax <- isAjaxRequest
    if isAjax then selectRep $ provideRep $ postEditNoteAJAX noteId
    else selectRep $ do
        provideRep $ postEditNoteHTML noteId note
        provideRep $ postEditNoteJSON noteId

postEditNoteAJAX :: NoteId -> Handler Html
postEditNoteAJAX noteId = do
    note <- updateNoteFromJson noteId
    ajaxContentLayout $(widgetFile "notes/view")

postEditNoteHTML :: NoteId -> Note -> Handler Html
postEditNoteHTML noteId note = do
    ((result, formWidget), formEnctype) <- runFormPost $ NoteForm.fromNote note
    case result of
        FormSuccess res -> do
            updateNote res noteId
            redirect $ NoteR noteId
        _ -> defaultLayout $ do
            setTitle "Edit note"
            let editNoteWidget = $(widgetFile "notes/editWidget")
            $(widgetFile "notes/edit")

postEditNoteJSON :: NoteId -> Handler Value
postEditNoteJSON noteId = do
    note <- updateNoteFromJson noteId
    returnJson note

updateNote :: NoteForm.NoteForm -> NoteId -> Handler ()
updateNote form noteId = runDB . update noteId $ [
        NoteTitle =. NoteForm.cleanTitle form,
        NoteContent =. NoteForm.content form,
        NotePublic =. NoteForm.public form
    ]

updateNoteFromJson :: NoteId -> Handler Note
updateNoteFromJson noteId = do
    res <- requireJsonBody :: Handler NoteForm.NoteForm
    updateNote res noteId
    runDB $ get404 noteId