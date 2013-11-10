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

import Text.Html

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

parseHTML str = filter (not . null) $ map liattribute $ parseTags str

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

genTable ::[ [(L.ByteString, L.ByteString)] ] -> Html
genTable items = 
    table ! [ border 1] <<
        ( besides theaders 
        </> (aboves . map ( tableRow )) items
        ) 
    where 
          theaders = map (th .toHtml) titles




liattribute (TagOpen "li" attrs) = attrs
liattribute _   = []

formPost = withSocketsDo $ withManager $ \m -> do
    resp <- flip httpLbs m =<<
                (formDataBody [partBS "id" "5"]
                $ fromJust $ parseUrl url )
    let body = responseBody resp
    return $ parseHTML body

toFile path  = do
    items <- formPost
    writeFile path $ show $ genTable items
