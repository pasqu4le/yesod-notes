{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.PublicNote where

import Import

getPublicNoteR :: NoteId -> Handler Html
getPublicNoteR noteId = do
    isAjax <- isAjaxRequest
    note <- runDB $ get404 noteId
    let confirmWidget = $(widgetFile "notes/confirm/public")
    if isAjax then ajaxContentLayout confirmWidget
    else defaultLayout $ do
        let noteWidget = $(widgetFile "notes/view")
        setTitle "Public note"
        $(widgetFile "notes/confirm/withNote")

postPublicNoteR :: NoteId -> Handler Html
postPublicNoteR noteId = do
    note <- runDB $ get404 noteId
    runDB $ update noteId [NotePublic =. not (notePublic note)]
    setMessage $ "Your note is now " ++ if notePublic note then "private" else "public"
    isAjax <- isAjaxRequest
    if isAjax then ajaxContentLayout $ toWidget [hamlet|<h1>Updated|]
    else redirect $ NoteR noteId