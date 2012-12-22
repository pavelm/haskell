import Network.HTTP
import System.IO
import Network.URI
import Data.Maybe
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Html.Generate(showattr)
import Data.Char
import Data.List

downloadURL :: String -> IO (Either String String)
downloadURL url = 
   do resp <- simpleHTTP request
      case resp of 
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
            case rspCode r of 
               (2,_,_) -> return $ Right (rspBody r)
               (3,_,_) -> 
                  case findHeader HdrLocation r of 
                     Nothing -> return $ Left (show r)
                     Just url -> downloadURL url
               _ -> return $ Left (show r)
   where request = Request { rqURI = uri,
                              rqMethod = GET,
                              rqHeaders = [],
                              rqBody = "" }
         uri = fromJust $ parseURI url

data NewsItem = NewsItem { itemTitle :: String,
                  url :: String }
            deriving (Eq, Show, Read)

data NewsFeed = Feed { channeltitle :: String,
                       items :: [NewsItem] }
            deriving (Eq, Show, Read)

parse :: String -> String -> NewsFeed
parse content name =
  NewsFeed { channeltitle = getTitle doc,
             items = getEnclosures doc }
  where parseResult = xmlParse name (stripUnicodeBOM content)
        doc = getContent parseResult 
      
        getContent :: Document -> Content
        getContent (Document _ _ e _) = CElem e

        stripUnicodeBOM :: String -> String
        stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) =
        stripUnicodeBOM x = x

getTitle :: Content -> String
getTitle doc = 
  contentToStringDefault "Untitled News Feed"
    (channel /> tag "title" /> txt $ doc)
