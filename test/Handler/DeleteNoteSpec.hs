{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.DeleteNoteSpec (spec) where

import TestImport
import Yesod.Form.Fields (Textarea(..))
import qualified Database.Persist.Class as P

spec :: Spec
spec = withApp $ do
    describe "Delete Note" $ do
        it "redirect anonymous users to login page" $ do
            userEntity <- createUser "foo"
            noteEntity <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = False
                }

            get $ DeleteNoteR (entityKey noteEntity)
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

            get $ DeleteNoteR (entityKey noteEntity)
            statusIs 403

        it "If a user owns a note let him delete it" $ do
            userEntity <- createUser "foo"
            noteEntity <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = False
                }

            authenticateAs userEntity
            let noteId = entityKey noteEntity

            get $ DeleteNoteR noteId
            statusIs 200

            post $ DeleteNoteR noteId
            statusIs 303

            mnote <- runDB $ P.get noteId
            assertEq "note was deleted" True $ isNothing mnote