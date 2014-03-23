{-# LANGUAGE OverloadedStrings #-}

module Model where

import qualified Data.Text as T
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
--import           Snap.Snaplet
--import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
--
--------------------------------------------------------------------------------
--import Application
------------------------------------------------------------------------------

data Link = Link {
    linkId :: Int
  , linkUrl :: T.Text
  , linkTitle :: T.Text
  , linkAuthor :: T.Text
  , linkAuthorId :: Int
} deriving Show

instance FromRow Link where
    fromRow = Link <$> field <*> field <*> field <*> field <*> field 

instance FromRow Int where
    fromRow = field


postLink :: (HasPostgres m) => T.Text -> T.Text -> Int -> m Int
postLink url title user = do
    linkids <- returning "INSERT INTO link (url,title,user_id) VALUES (?, ?, ?) RETURNING id" [(url, title, user)]
    return $ head linkids
