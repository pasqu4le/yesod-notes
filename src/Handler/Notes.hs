{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Notes where

import Import
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
        let newNoteWidget = $(widgetFile "notes/add")
        setTitle "My Notes"
        $(widgetFile "notes/list")

noteTileWidget :: Note -> NoteId -> Widget
noteTileWidget note noteId = $(widgetFile "notes/view")