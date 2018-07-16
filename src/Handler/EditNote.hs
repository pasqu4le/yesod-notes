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
    let editNoteWidget = $(widgetFile "notes/editWidget")
    isAjax <- isAjaxRequest
    if isAjax then ajaxContentLayout editNoteWidget
    else defaultLayout $ do
        setTitle "Edit note"
        $(widgetFile "notes/edit")

postEditNoteR :: NoteId -> Handler TypedContent
postEditNoteR noteId = do
    isAjax <- isAjaxRequest
    if isAjax then selectRep $ provideRep $ postEditNoteAJAX noteId
    else selectRep $ do
        provideRep $ postEditNoteHTML noteId
        provideRep $ postEditNoteJSON noteId

postEditNoteAJAX :: NoteId -> Handler Html
postEditNoteAJAX noteId = do
    _ <- runDB $ get404 noteId
    res <- (requireJsonBody :: Handler NoteForm.NoteForm)
    runDB $ update noteId [NoteTitle =. NoteForm.cleanTitle res, NoteContent =. NoteForm.content res]
    note <- runDB $ get404 noteId 
    ajaxContentLayout $(widgetFile "notes/view")

postEditNoteHTML :: NoteId -> Handler Html
postEditNoteHTML noteId = do
    note <- runDB $ get404 noteId
    ((result, formWidget), formEnctype) <- runFormPost (NoteForm.fromNote note)
    case result of
        FormSuccess res -> do
            runDB $ update noteId [NoteTitle =. NoteForm.cleanTitle res, NoteContent =. NoteForm.content res]
            redirect $ NoteR noteId
        _ -> defaultLayout $ do
            setTitle "Edit note"
            let editNoteWidget = $(widgetFile "notes/editWidget")
            $(widgetFile "notes/edit")

postEditNoteJSON :: NoteId -> Handler Value
postEditNoteJSON noteId = do
    _ <- runDB $ get404 noteId
    res <- (requireJsonBody :: Handler NoteForm.NoteForm)
    runDB $ update noteId [NoteTitle =. NoteForm.cleanTitle res, NoteContent =. NoteForm.content res]
    note <- runDB $ get404 noteId
    returnJson note
