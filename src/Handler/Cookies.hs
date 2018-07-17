{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Cookies where

import Import
import qualified Data.Map.Strict as Map

getCookiesR :: Handler Html
getCookiesR = do
    sess <- getSession
    let hasUsrCk = Map.member "_ID" sess
        hasMsgCk = Map.member "_MSG" sess
        hasUltCk = Map.member "_ULT" sess
        hasCokCk = Map.member cookiesKey sess
    defaultLayout $ do
        setTitle "Cookies"
        $(widgetFile "cookies/page")

postCookiesR :: Handler Html
postCookiesR = do
    setSession cookiesKey "true"
    isAjax <- isAjaxRequest
    if isAjax then return "" else redirectUltDest HomeR
