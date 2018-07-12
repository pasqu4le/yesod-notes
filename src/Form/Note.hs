{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Form.Note where

import Import
import qualified Form.Bulma as Bulma
import Data.Aeson (FromJSON(..), withObject) 

data NoteForm = NoteForm
    { title :: Maybe Text
    , content :: Textarea
    }

instance FromJSON NoteForm where
    parseJSON = withObject "NoteForm" $ \v -> NoteForm
        <$> v .: "title"
        <*> v .: "content"

empty :: Form NoteForm
empty = Bulma.render $ NoteForm
    <$> aopt textField (Bulma.inputSetting "Title") Nothing
    <*> areq textareaField (Bulma.textareaSetting "Content") Nothing

fromNote :: Note -> Form NoteForm
fromNote note = Bulma.render $ NoteForm
    <$> aopt textField (Bulma.inputSetting "Title") (Just $ noteTitle note)
    <*> areq textareaField (Bulma.textareaSetting "Content") (Just . Textarea $ noteContent note)