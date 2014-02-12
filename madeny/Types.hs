{-# LANGUAGE OverloadedStrings #-}
module Types  where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text as T

import Search.ElasticSearch

data Page = Page 
    { pgTitle :: String
    , pgUrl :: String
    , pgContents :: String
    , pgLinks :: [ String ]
    } deriving (Show,Eq)

instance FromJSON Page where
    parseJSON (Object v)= Page <$>
                           v .: "title" <*>
                           v .: "url" <*>
                           v .: "contents" <*>
                           v .: "links"
    parseJSON _         = mzero

instance ToJSON Page where
    toJSON (Page t u c l) = object [ "title" .= t, "url" .= u, "contents" .= c, "links" .= toJSON l ]


instance Document Page where 
    documentKey p = T.pack $ pgUrl p
    documentType = DocumentType "page"
