{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    muid <- maybeAuthId
    defaultLayout $ do
        setTitle "Welcome To Yesod-Notes"
        $(widgetFile "homepage")
