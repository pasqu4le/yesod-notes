{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Notes where

import Import
import Text.Julius (RawJS (..))
import qualified Form.Note as NoteForm

getNotesR :: Handler TypedContent
getNotesR = do
    uid <- requireAuthId
    noteList <- runDB $ selectList [NoteUserId ==. uid] [Desc NoteId]
    selectRep $ do
        provideRep $ getNotesHTML noteList
        provideJson noteList

getNotesHTML :: [Entity Note] -> Handler Html
getNotesHTML noteList = do
    (formWidget, formEnctype) <- generateFormPost NoteForm.empty
    defaultLayout $ do
        let noteListId = "noteList" :: Text
            newNoteFormId = "js-newNoteForm" :: Text
            newNoteWidget = $(widgetFile "notes/new")
        setTitle "My Notes"
        $(widgetFile "notes/list")

noteTileWidget :: Note -> NoteId -> Widget
noteTileWidget note noteId = $(widgetFile "notes/view")