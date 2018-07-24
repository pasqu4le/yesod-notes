{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.CookiesSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Cookies Page" $ do
        it "check cookie page for active keys" $ do
            get CookiesR
            statusIs 200
            htmlNoneContain "span.tag" "Active"

            userEntity <- createUser "foo"
            authenticateAs userEntity
            
            get CookiesR
            statusIs 200
            htmlAnyContain "span.tag" "Active"