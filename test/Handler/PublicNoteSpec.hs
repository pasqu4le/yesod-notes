{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.PublicNoteSpec (spec) where

import TestImport
import Yesod.Form.Fields (Textarea(..))

spec :: Spec
spec = withApp $ do
    describe "Change Note Visibility" $ do
        it "redirect anonymous users to login page" $ do
            userEntity <- createUser "foo"
            noteEntity <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = False
                }

            get $ PublicNoteR (entityKey noteEntity)
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

            get $ PublicNoteR (entityKey noteEntity)
            statusIs 403

        it "If a user owns a note let him change it's visibility" $ do
            userEntity <- createUser "foo"
            noteEntity <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = False
                }

            authenticateAs userEntity
            let noteId = entityKey noteEntity

            get $ PublicNoteR noteId
            statusIs 200

            post $ PublicNoteR noteId
            statusIs 303

            changedNote <- runDB $ getJust noteId
            assertEq "note was made public" True $ notePublic changedNote

            post $ PublicNoteR noteId
            statusIs 303

            reChangedNote <- runDB $ getJust noteId
            assertEq "note was made public" False $ notePublic reChangedNote

