{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.ArticleList where

import Import
import Data.Text (Text)
import Prelude hiding (getLine, putStrLn)
import Data.Text

getArticleListR :: Handler Html
getArticleListR = do
    articles <- runDB $ selectList [] [Desc ArticlePublished]
    defaultLayout $(widgetFile "articleList")

--削除処理のポスト
postArticleDeleteR :: Handler Html
postArticleDeleteR = do
  articleIdText <- runInputPost $ ireq hiddenField "articleId"
  case (fromPathPiece articleIdText) of
    Just articleId -> do
      _ <- runDB $ delete (articleId :: ArticleId)
      redirect ArticleListR
    Nothing -> Import.undefined
    _ -> Import.undefined
