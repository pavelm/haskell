{-# LANGUAGE TupleSections #-}
module XstUtil where

import Control.Concurrent
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import Data.Text(pack)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.UTF8(toString)
import Data.Time
import Network
import Network.HTTP.Conduit 
import Network.HTTP.Conduit.MultipartFormData
import Network.Curl
import Network.Curl.Opts
import Network.Shpider.Code
import Network.Shpider.URL
import Search.ElasticSearch
import Text.HandsomeSoup
import Text.Regex.Posix
import Text.XML.HXT.Core    -- basic HXT stuff
import Text.XML.HXT.Curl                -- Curl HTTP handler
import Text.XML.HXT.XPath               -- additional XPath functions
import Types
import qualified Data.Sequence as Q
import qualified Data.Set as S
import qualified Text.XML.HXT.DOM.XmlNode as XN

data CrawlerState = 
    CS { toVisit :: Q.Seq (Int,String)
       , seen :: S.Set String
       , dontLeaveDomain :: Bool
       , currentPage :: Page
       , pageCount :: !Int
       , curlOpts :: [CurlOption]
       , lastDownloadTime :: !(Maybe UTCTime)
       , downloadThrottle :: Maybe Int
       }
    deriving Show

type Crawler = StateT CrawlerState IO

emptyPage = Page "" "" "" []

initialState seeds = 
    CS { toVisit = Q.fromList seeds
       , seen = S.empty
       , dontLeaveDomain = True
       , currentPage = emptyPage
       , pageCount = 0
       , lastDownloadTime = Nothing
       , curlOpts = [CurlTimeout 10]
       , downloadThrottle = Just 500
       }

runCrawler seeds f = do
    withCurlDo $ runStateT f (initialState seeds) 

markSeen :: String -> Crawler ()
markSeen url = do 
    crawler <- get 
    put $ crawler { seen = S.insert url (seen crawler) }


download :: Int -> String -> Crawler (ShpiderCode, Page)
download depth url = do 
    lift $ putStrLn $ "visiting " ++ url ++ " at depth " ++ show depth
    res@(c,page) <- downloadAPage url
    markSeen $ pgUrl page
    crawler <- get
    let visited = seen crawler
        inQueue = toVisit crawler
        stayOnDomain = dontLeaveDomain crawler
        followLinks = S.fromList $ pgLinks page
        linksToAddSet = S.difference followLinks visited
        linksToAddList = if depth == 0 then [] else (S.toList linksToAddSet)
        linksToAdd = Q.fromList $ map (\l -> (depth-1,l)) $ filter (\l -> not stayOnDomain || isSameDomain url l ) linksToAddList 
    do lift $ 
        if page == emptyPage then 
            putStrLn ("empty page " ++ url) 
        else 
            indexDocument localServer "crawler" page
    put $ crawler { toVisit = (Q.drop 1 inQueue) Q.>< linksToAdd }
    return res

crawlerLoop = do 
    links <- gets toVisit
    visited <- gets seen
    let (depth,url) = Q.index links 0 
        count = Q.length links
    if count > 0 && depth >= 0 then do 
        (code, page) <- download depth url
        do lift $ putStrLn $ "total to visit " ++ show count
        crawlerLoop 
    else 
        return ()
    


downloadAPage :: String -> Crawler (ShpiderCode, Page)
downloadAPage url = do 
    crawler <- get 
    if isHttp url then do
        response <- withThrottle $ liftIO $ curlGetResponse_ url (curlOpts crawler)
        maybe (mkRes url (respCurlCode response, respBody response))
              ( \ ct -> 
                    if validContentType ct 
                    then mkRes url (CurlOK, respBody response)
                    else return (WrongData, emptyPage)
              )
              (lookup ("Content-Type" :: String) $ respHeaders response)
    else getURL url


mkRes :: String -> (CurlCode, String) -> Crawler (ShpiderCode, Page)
mkRes url (curlCode, html) = do
    p <- if curlCode == CurlOK
         then liftIO $ parsePage url html
         else return emptyPage 
    return (ccToSh curlCode, p)


formPost = withSocketsDo $ withManager $ \m -> do
    resp <- flip httpLbs m =<<
                (formDataBody [partBS (pack "id") (BS.pack "5")]
                $ fromJust $ parseUrl "http://mappedinny.com/_php/get_companies.php")
    let body = responseBody resp
    return $ toString body

parsePage root html = do 
    title <- runX $ doc >>> css "title" >>> deep getText
    contents <- runX $ doc >>> processTopDown (filterA $ neg (hasName "script")) >>> css "body" >>> removeAllWhiteSpace >>> removeAllComment //> getText
    links <- runX $ doc >>> css "a" ! "href"
    return $ Page (headOrDefault "" title) root (concat contents) (absLinks links)
    where doc = setDefaultBaseURI root >>> readString [withParseHTML yes, withWarnings no ] html
          absLinks ls = mapMaybe (\l -> expandURIString l root) ls
          headOrDefault value [] = value
          headOrDefault _ xs = head xs

withAuthorizedDomain :: String -> String -> Crawler( ShpiderCode , Page ) -> Crawler( ShpiderCode , Page )
withAuthorizedDomain startPage url f = do
   shpider <- get
   if dontLeaveDomain shpider
   then do
         if isSameDomain startPage url 
         then f
         else return ( OffSite , emptyPage )
   else f

withThrottle :: Crawler a -> Crawler a
withThrottle f = do
  let perform = do
        res <- f
        sh <- get
        now <- liftIO $ getCurrentTime
        put $ sh { lastDownloadTime = Just now }
        return res
  thOpt <- gets downloadThrottle
  lastD <- gets lastDownloadTime
  case thOpt of
    Nothing -> perform
    Just n -> do
      case lastD of 
        Nothing -> perform
        Just ld -> do
          th <- liftIO $ shouldThrottle n ld
          case th of
            Just x -> liftIO (threadDelay x) >> perform
            Nothing -> perform

shouldThrottle :: Int -> UTCTime -> IO (Maybe Int)
shouldThrottle n lastTime = do
  now <- getCurrentTime
  let n' = fromIntegral n / 1000000
      diff = diffUTCTime now lastTime
      delta = round . (* 1000000) $ n' - diff
  return $  if delta > 0 then (Just delta) else Nothing

validContentType :: String -> Bool
validContentType ct =
   or $ map ( \ htmlct -> ct =~ htmlct ) htmlContentTypes

getURL :: String -> Crawler (ShpiderCode, Page)
getURL url = withThrottle $ do
    shpider <- get
    contents <- liftIO $ curlGetString_ url (curlOpts shpider)
    mkRes url contents

htmlContentTypes = [ "text/html" , "application/xhtml+xml" ]

crawlJobs = do
    html <- formPost
    let doc = parseHtml html
    lis <- runX $ doc >>> css "li" ! "data-hiringurl"
    let urls = map (1,) $ filter (not . null) lis
    runCrawler urls $ do 
        crawlerLoop

run seed = runCrawler seed $ do 
    s <-get 
    crawlerLoop
