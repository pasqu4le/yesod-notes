{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.EditNote where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

data NoteForm = NoteForm
    { noteFormTitle :: Maybe Text
    , noteFormContent :: Textarea
    }

getEditNoteR :: NoteId -> Handler Html
getEditNoteR noteId = do
    note <- runDB $ get404 noteId
    (formWidget, formEnctype) <- generateFormPost (noteForm note)
    defaultLayout $ do
        setTitle "Edit note"
        $(widgetFile "notes/edit")

postEditNoteR :: NoteId -> Handler Html
postEditNoteR noteId = do
    note <- runDB $ get404 noteId
    ((result, formWidget), formEnctype) <- runFormPost (noteForm note)
    case result of
        FormSuccess res -> do
            runDB $ update noteId [NoteTitle =. noteFormTitle res, NoteContent =. unTextarea (noteFormContent res)]
            redirect $ NoteR noteId
        _ -> defaultLayout $ do
            setTitle "Edit note"
            $(widgetFile "notes/edit")

noteForm :: Note -> Form NoteForm
noteForm note = renderBootstrap3 BootstrapBasicForm $ NoteForm
    <$> aopt textField (bfs ("Title" :: Text)) (Just $ noteTitle note)
    <*> areq textareaField (bfs ("Content" :: Text)) (Just . Textarea $ noteContent note)