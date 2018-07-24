{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.EditNoteSpec (spec) where

import TestImport
import Yesod.Form.Fields (Textarea(..), unTextarea)
import qualified Database.Persist.Class as P

spec :: Spec
spec = withApp $ do
    describe "Edit Note" $ do
        it "redirect anonymous users to login page" $ do
            userEntity <- createUser "foo"
            noteEntity <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = False
                }

            get $ EditNoteR (entityKey noteEntity)
            location <- getLocation
            assertEq "redirected to login page" (Right (AuthR LoginR)) location

        it "Do not authorize users if they don't own the note" $ do
            userEntity <- createUser "foo"
            noteEntity <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = False
                }

            otherUserEntity <- createUser "another"
            authenticateAs otherUserEntity

            get $ EditNoteR (entityKey noteEntity)
            statusIs 403

        it "If a user owns a note let him edit it" $ do
            userEntity <- createUser "foo"
            noteEntity <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = False
                }

            authenticateAs userEntity
            let noteId = entityKey noteEntity

            get $ EditNoteR noteId
            statusIs 200

            let newContent = Textarea "baz"
            request $ do
                setMethod "POST"
                setUrl $ EditNoteR noteId
                addToken
                addPostParam "f2" $ unTextarea newContent
            statusIs 303

            mnote <- runDB $ P.get noteId
            assertEq "note was modified" True $ case mnote of
                Just note -> newContent == noteContent note
                _ -> False
