{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Hello where

import Import
--import Data.Text

getHelloR :: Handler Html
--getHelloR = defaultLayout $(widgetFile "hello")
getHelloR = do
    let
        message = "ハンドラーで定義したメッセージです。" :: Text 
        list = [1..10] :: [Int]
        x = Nothing :: Maybe Bool
    defaultLayout $(widgetFile "hello")
