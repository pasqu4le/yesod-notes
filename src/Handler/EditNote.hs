{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.EditNote where

import Import
import qualified Form.Note as NoteForm

getEditNoteR :: NoteId -> Handler Html
getEditNoteR noteId = do
    note <- runDB $ get404 noteId
    (formWidget, formEnctype) <- generateFormPost (NoteForm.fromNote note)
    defaultLayout $ do
        setTitle "Edit note"
        $(widgetFile "notes/edit")

postEditNoteR :: NoteId -> Handler TypedContent
postEditNoteR noteId = selectRep $ do
    provideRep $ postEditNoteHTML noteId
    provideRep $ postEditNoteJSON noteId

postEditNoteHTML :: NoteId -> Handler Html
postEditNoteHTML noteId = do
    note <- runDB $ get404 noteId
    ((result, formWidget), formEnctype) <- runFormPost (NoteForm.fromNote note)
    case result of
        FormSuccess res -> do
            runDB $ update noteId [NoteTitle =. NoteForm.title res, NoteContent =. unTextarea (NoteForm.content res)]
            redirect $ NoteR noteId
        _ -> defaultLayout $ do
            setTitle "Edit note"
            $(widgetFile "notes/edit")

postEditNoteJSON :: NoteId -> Handler Value
postEditNoteJSON noteId = do
    _ <- runDB $ get404 noteId
    res <- (requireJsonBody :: Handler NoteForm.NoteForm)
    updatedNote <- runDB $ update noteId [NoteTitle =. NoteForm.title res, NoteContent =. unTextarea (NoteForm.content res)]
    returnJson updatedNote
