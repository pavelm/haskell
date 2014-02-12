{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit 
import Network.HTTP.Conduit.MultipartFormData
import Network

import qualified Data.ByteString.Lazy as L 

import Data.Text.Encoding as TE
import Control.Monad
import Data.Maybe
import Data.ByteString.Lazy.UTF8(toString)
import Text.HTML.TagSoup

import Text.Html (toHtml,(<->),HtmlTable,td, (</>), (<<), hotlink, table, border, besides, aboves, th, td, Html)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Text (pack)

import Search.ElasticSearch
import HXTUtil
import Types


url = "http://mappedinny.com/_php/get_companies.php" 
main = simpleHttp url >>= L.putStr

{-
formPost = do 
    request <- parseUrl url
    let req = request { method = "POST" }
    formDataBody [partBS "id" "5"] req >>= \r -> 
        withManager $ \manager -> do
            response <- http r manager
            return $ responseBody response 
-}

--parseHTML str = filter (not . null) $ map liattribute $ parseTags str
{- 
parseHTML = dropNotNull . attributes . parseTags 
    where dropNotNull = filter (not . null)
          attributes = map liattribute
-}

headers :: [String]
headers = ["data-name","data-address","data-url","data-hiringurl", "data-whynyc"]

titles :: [String]
titles = ["Company","Address", "Home", "Jobs Page","Why NYC?"]


tableRow ::[(L.ByteString, L.ByteString) ] -> HtmlTable
tableRow row = company <-> address <-> homepage <-> jobspage <-> whynyc
    where col wrapper field = 
            case lookup field row of
                Just v     ->  td $ wrapper v
                Nothing    -> td $ toHtml $ toString ""
          simple = col (toHtml . toString)
          link alt v = toHtml $ hotlink url [toHtml linktext]
            where url = toString v
                  linktext = if length url > 50 then alt else url
          url = col (\v -> link "Jobs" v)
          company = simple "data-name"
          address = simple "data-address"
          homepage = url "data-url"
          jobspage = url "data-hiringurl"
          whynyc = simple "data-whynyc"

{-
genTable ::[ [(L.ByteString, L.ByteString)] ] -> Html
genTable items = 
    table ! [ border 1] <<
        ( besides theaders 
        </> (aboves . map ( tableRow )) items
        ) 
    where 
          theaders = map (th .toHtml) titles
          -}
genTable items = undefined

liattribute (TagOpen "li" attrs) = attrs
liattribute _   = []

formPost = withSocketsDo $ withManager $ \m -> do
    resp <- flip httpLbs m =<<
                (formDataBody [partBS "id" "5"]
                $ fromJust $ parseUrl url )
    let body = responseBody resp
    return $ body

{-
toFile path  = do
    items <- formPost
    writeFile path $ show $ genTable items
  -}  

parsePage root html = do 
    title <- runX $ doc >>> css "title" >>> deep getText
    contents <- runX $ doc >>> processTopDown (filterA $ neg (hasName "script")) >>> css "body" >>> removeAllWhiteSpace >>> removeAllComment //> getText
    links <- runX $ doc >>> css "a" ! "href"
    return $ Page (pack $ head title) root (pack $ concat contents) (map pack links)
    where doc = setDefaultBaseURI "http://google.com" >>> readString [withParseHTML yes, withWarnings no ] html


--runX $ doc >>> css "li" ! "data-hiringurl"
--
