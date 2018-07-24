{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.NoteSpec (spec) where

import TestImport
import Yesod.Form.Fields (Textarea(..))

spec :: Spec
spec = withApp $ do
    describe "View Note" $ do
        it "allow anyone see a note if it's public" $ do
            userEntity <- createUser "foo"
            noteEntity <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = True
                }

            get $ NoteR (entityKey noteEntity)
            statusIs 200

            authenticateAs userEntity

            get $ NoteR (entityKey noteEntity)
            statusIs 200
        it "redirect anonymous users to login page if the note is private" $ do
            userEntity <- createUser "foo"
            noteEntity <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = False
                }

            get $ NoteR (entityKey noteEntity)
            statusIs 303

        it "allow only it's owner to see a note if it's private" $ do
            userEntity <- createUser "foo"
            noteEntity <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = False
                }

            otherUserEntity <- createUser "another"
            authenticateAs otherUserEntity
            get $ NoteR (entityKey noteEntity)
            statusIs 403

            authenticateAs userEntity
            get $ NoteR (entityKey noteEntity)
            statusIs 200