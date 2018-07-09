{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.EditNote where

import Import
import qualified Form.Bulma as Bulma

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

-- TODO: take a look at this
noteForm :: Note -> Form NoteForm
noteForm note = Bulma.render $ NoteForm
    <$> aopt textField (Bulma.inputSetting "Title") (Just $ noteTitle note)
    <*> areq textareaField (Bulma.textareaSetting "Content") (Just . Textarea $ noteContent note)
