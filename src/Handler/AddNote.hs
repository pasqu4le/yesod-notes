{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.AddNote where

import Import
import qualified Form.Bulma as Bulma
import Data.Aeson (withObject)

data NoteForm = NoteForm
    { noteFormTitle :: Maybe Text
    , noteFormContent :: Textarea
    }

instance FromJSON NoteForm where
    parseJSON = withObject "NoteForm" $ \v -> NoteForm
        <$> v .: "title"
        <*> v .: "content"

getAddNoteR :: Handler Html
getAddNoteR = do
    (formWidget, formEnctype) <- generateFormPost noteForm
    defaultLayout $ do
        setTitle "Add a note"
        $(widgetFile "notes/add")

postAddNoteR :: Handler TypedContent
postAddNoteR = do
    uid <- requireAuthId
    selectRep $ do
        provideRep $ do
            ((result, formWidget), formEnctype) <- runFormPost noteForm
            case result of
                FormSuccess res -> do
                    let newNote = Note (noteFormTitle res) (uid) (unTextarea $ noteFormContent res)
                    note <- runDB $ insertEntity newNote
                    redirect $ NoteR (entityKey note)
                _ -> defaultLayout $ do
                    setTitle "Add a note!"
                    $(widgetFile "notes/add")
        provideRep $ do
            res <- (requireJsonBody :: Handler NoteForm)
            let note = Note (noteFormTitle res) (uid) (unTextarea $ noteFormContent res)
            insertedNote <- runDB $ insertEntity note
            returnJson insertedNote


noteForm :: Form NoteForm
noteForm = Bulma.render $ NoteForm
    <$> aopt textField (Bulma.inputSetting "Title") Nothing
    <*> areq textareaField (Bulma.textareaSetting "Content") Nothing