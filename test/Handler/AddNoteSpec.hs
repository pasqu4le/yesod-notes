{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.AddNoteSpec (spec) where

import TestImport
import qualified Database.Persist.Class as P
import Yesod.Form.Fields (Textarea(..), unTextarea)

spec :: Spec
spec = withApp $ do
    describe "Add Note" $ do
        it "redirect anonymous users to login page" $ do
            get AddNoteR
            location <- getLocation
            assertEq "redirected to login page" (Right (AuthR LoginR)) location

        it "authenticated users can access the page and add a note" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            get AddNoteR
            statusIs 200

            let newContent = Textarea "baz"
            request $ do
                setMethod "POST"
                setUrl AddNoteR
                addToken
                addPostParam "f2" $ unTextarea newContent

            location <- getLocation
            case location of
                Right (NoteR noteId) -> do
                    mnote <- runDB $ P.get noteId
                    assertEq "note was inserted" True $ case mnote of
                        Just note -> newContent == noteContent note
                        _ -> False
                _ -> error "was not redirected to new note page"