{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.AddNote where

import Import
import qualified Form.Note as NoteForm

getAddNoteR :: Handler Html
getAddNoteR = do
    (formWidget, formEnctype) <- generateFormPost NoteForm.empty
    defaultLayout $ do
        setTitle "Add a note"
        $(widgetFile "notes/add")

postAddNoteR :: Handler TypedContent
postAddNoteR = do
    uid <- requireAuthId
    isAjax <- isAjaxRequest
    if isAjax then
        selectRep $ provideRep $ postAddNoteAJAX uid
    else
        selectRep $ do
            provideRep $ postAddNoteHTML uid
            provideRep $ postAddNoteJSON uid

postAddNoteHTML :: UserId -> Handler Html
postAddNoteHTML uid = do
    ((result, formWidget), formEnctype) <- runFormPost NoteForm.empty
    setUltDestReferer
    case result of
        FormSuccess res -> do
            note <- insertNote res uid
            redirect $ NoteR (entityKey note)
        _ -> defaultLayout $ do
            setTitle "Add a note!"
            $(widgetFile "notes/add")

postAddNoteJSON :: UserId -> Handler Value
postAddNoteJSON uid = do
    res <- requireJsonBody :: Handler NoteForm.NoteForm
    newNote <- insertNote res uid
    returnJson $ entityVal newNote

postAddNoteAJAX :: UserId -> Handler Html
postAddNoteAJAX uid = do
    res <- requireJsonBody :: Handler NoteForm.NoteForm
    newNote <- insertNote res uid
    let noteId = entityKey newNote
        note = entityVal newNote
    ajaxContentLayout $ do
        let noteWidget = $(widgetFile "notes/view")
        [whamlet|
            <div .column.is-narrow>
                ^{noteWidget}
        |]

insertNote :: NoteForm.NoteForm -> UserId -> Handler (Entity Note)
insertNote form uid = runDB . insertEntity $ Note {
        noteTitle = NoteForm.cleanTitle form,
        noteUserId = uid, 
        noteContent = NoteForm.content form,
        notePublic = NoteForm.public form
    }