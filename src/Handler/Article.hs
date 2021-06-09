{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handler.Article where

import Import
import Data.Text (Text)
import Prelude hiding (getLine, putStrLn)
import Data.Text

import Data.Text ()
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), BootstrapGridOptions (..),
                              renderBootstrap3)

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH                              

-- フォーム取得
articleForm :: Maybe Article -> Html -> MForm Handler (FormResult Article, Widget)
articleForm article extra = do
  (titleResult, titleView)            <- mreq textField "タイトル" (articleTitle <$> article)
  (publishedResult, publishedView)    <- mreq dayField "公開日" (articlePublished <$> article)
  (viewCountResult, viewCountView)    <- mopt intField "ビュー数" (articleViewCount <$> article)
  let result = Article
               <$> titleResult
               <*> publishedResult
               <*> viewCountResult
      widget = $(widgetFile "article-editor-form")
  return (result, widget)

-- 新規登録
getArticleR :: Handler Html
getArticleR = do
  let
    header = "Article新規登録" :: Text
  (widget, enctype) <- generateFormPost $ articleForm Nothing
  defaultLayout $(widgetFile "article")

-- 新規登録処理のポスト
postArticleR :: Handler Html
postArticleR = do
  ((result, widget),enctype) <- runFormPost $ articleForm Nothing 
  let header = "Article新規登録" :: Text 
  case result of 
    FormSuccess article -> do
      -- Postされたデータが正常な場合
      articleId <- runDB $ insert article 
      redirect ArticleListR 
    FormFailure _ -> do
      -- 不正な入力値のデータが送信された場合(必須項目が未入力等)
      setMessage "不正なデータが送信されました。"
      defaultLayout $(widgetFile "article")
    FormMissing -> do
      -- defaultLayout [whamlet|データが送信されませんでした。 |] -> undefined
      defaultLayout $(widgetFile "article")

--更新
getArticleUpdateR :: ArticleId -> Handler Html
getArticleUpdateR articleId = do
  let
    header = "Article更新" :: Text
  article <- runDB $ get404 articleId  --更新対象のデータが存在しないと404エラー
  (widget, enctype) <- generateFormPost $ articleForm $ Just article
  defaultLayout $(widgetFile "articleUpdate")

--更新処理のポスト
postArticleUpdateR :: ArticleId -> Handler Html
postArticleUpdateR articleId = do
  ((result, widget),enctype) <- runFormPost $ articleForm Nothing
  let
    header = "Article更新" :: Text
  case result of
    FormSuccess article -> do
      --Postされたデータが正常な場合
      _ <- runDB $ do
        _ <- get404 articleId	   --更新対象のデータが存在しないと404エラー
        Database.Persist.Sqlite.replace articleId article   --更新処理の実行
      redirect ArticleListR
    FormFailure _ -> do
      --不正な入力値のデータが送信された場合(必須項目が未入力等)
      setMessage "不正なデータが送信されました。"
      defaultLayout $(widgetFile "article")
    FormMissing -> defaultLayout [whamlet|データが送信されませんでした。 |]
    _ -> Import.undefined
