{-# LANGUAGE OverloadedStrings #-}

module Model where 

import qualified Data.Text as T 
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple


data Link = Link {
    linkId :: Int
  , linkUrl :: T.Text
  , linkTitle :: T.Text
  , linkAuthor :: T.Text
  , linkAuthorId :: Int
} deriving Show

instance FromRow Link where
    fromRow = Link <$> field <*> field <*> field <*> field <*> field 
