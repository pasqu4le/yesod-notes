{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.NotesSpec (spec) where

import TestImport
import Yesod.Form.Fields (Textarea(..))

spec :: Spec
spec = withApp $ do
    describe "View all notes" $ do
        it "redirect anonymous users to login page" $ do
            get NotesR
            location <- getLocation
            assertEq "redirected to login page" (Right (AuthR LoginR)) location

        it "shows an authenticated user his notes, if he has any" $ do
            userEntity <- createUser "foo"
            runDB $ deleteWhere [NoteUserId ==. (entityKey userEntity)]

            authenticateAs userEntity
            get NotesR
            htmlCount "#note-list > .column" 0

            _ <- runDB . insertEntity $ Note {
                    noteTitle = Nothing,
                    noteUserId = (entityKey userEntity), 
                    noteContent = Textarea "bar",
                    notePublic = False
                }

            get NotesR
            htmlCount "#note-list > .column" 1