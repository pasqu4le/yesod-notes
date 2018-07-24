{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Homepage" $ do
        it "check the status and some of it's content" $ do
            get HomeR
            statusIs 200
            htmlAnyContain "h1.title" "Yesod—notes"
