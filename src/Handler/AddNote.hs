{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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

-- TODO: check if is Ajax with X-Requested-With
postAddNoteR :: Handler TypedContent
postAddNoteR = do
    isAjax <- isAjaxRequest
    if isAjax then
        selectRep $ provideRep postAddNoteAJAX
    else
        selectRep $ do
            provideRep postAddNoteHTML
            provideRep postAddNoteJSON

postAddNoteHTML :: Handler Html
postAddNoteHTML = do
    uid <- requireAuthId
    ((result, formWidget), formEnctype) <- runFormPost NoteForm.empty
    setUltDestReferer
    case result of
        FormSuccess res -> do
            let newNote = Note (NoteForm.title res) (uid) (unTextarea $ NoteForm.content res)
            note <- runDB $ insertEntity newNote
            redirect $ NoteR (entityKey note)
        _ -> defaultLayout $ do
            setTitle "Add a note!"
            $(widgetFile "notes/add")

postAddNoteJSON :: Handler Value
postAddNoteJSON = do
    uid <- requireAuthId
    res <- (requireJsonBody :: Handler NoteForm.NoteForm)
    let note = Note (NoteForm.title res) (uid) (unTextarea $ NoteForm.content res)
    insertedNote <- runDB $ insertEntity note
    returnJson insertedNote

postAddNoteAJAX :: Handler Html
postAddNoteAJAX = do
    uid <- requireAuthId
    res <- (requireJsonBody :: Handler NoteForm.NoteForm)
    let note = Note (NoteForm.cleanTitle res) (uid) (unTextarea $ NoteForm.content res)
    insertedNote <- runDB $ insertEntity note
    let noteId = entityKey insertedNote
    ajaxContentLayout $ do
        let noNoteButtons = False
            noteWidget = $(widgetFile "notes/view")
        [whamlet|
            <div .column.is-narrow>
                ^{noteWidget}
        |]
