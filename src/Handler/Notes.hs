{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Notes where

import Import
import Text.Julius (RawJS (..))
import Handler.AddNote (noteForm)

getNotesR :: Handler Html
getNotesR = do
    uid <- requireAuthId
    noteList <- runDB $ selectList [NoteUserId ==. uid] [Desc NoteId]
    (formWidget, formEnctype) <- generateFormPost noteForm
    defaultLayout $ do
        let noteListId = "noteList" :: Text
            newNoteFormId = "js-newNoteForm" :: Text
            newNoteWidget = $(widgetFile "notes/new")
        setTitle "My Notes"
        $(widgetFile "notes/list")

noteTileWidget :: Note -> NoteId -> Widget
noteTileWidget note noteId = $(widgetFile "notes/view")
    where noNoteButtons = False