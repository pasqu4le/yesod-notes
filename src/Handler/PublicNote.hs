{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.PublicNote where

import Import

getPublicNoteR :: NoteId -> Handler TypedContent
getPublicNoteR noteId = do
    isAjax <- isAjaxRequest
    note <- runDB $ get404 noteId
    let confirmWidget = $(widgetFile "notes/confirm/public")
    if isAjax then selectRep $ provideRep $ ajaxContentLayout confirmWidget
    else selectRep $ do
        provideRep $ defaultLayout $ do
            let noteWidget = $(widgetFile "notes/view")
            setTitle "Public note"
            $(widgetFile "notes/confirm/withNote")
        provideJson $ notePublic note

postPublicNoteR :: NoteId -> Handler TypedContent
postPublicNoteR noteId = do
    note <- runDB $ get404 noteId
    runDB $ update noteId [NotePublic =. not (notePublic note)]
    setMessage $ "Your note is now " ++ if notePublic note then "private" else "public"
    isAjax <- isAjaxRequest
    if isAjax then selectRep $ provideRep $ ajaxContentLayout $ toWidget [hamlet|<h1>Updated|]
    else selectRep $ do
        provideRep $ (redirect (NoteR noteId) :: Handler Html)
        provideJson $ ("success: note visibility changed" :: Text)