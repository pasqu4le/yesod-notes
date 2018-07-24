{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Profile page" $ do
        it "redirect anonymous users to login page" $ do
            get ProfileR
            location <- getLocation
            assertEq "redirected to login page" (Right (AuthR LoginR)) location

        it "authenticated users will receive their profile page" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            get ProfileR
            statusIs 200
            htmlAnyContain "h1" "Hey there!"
