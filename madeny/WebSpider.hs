{-# LANGUAGE ScopedTypeVariables #-}
module Main where
 
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import System.Environment
import Control.Exception
import Network.HTTP
import Network.URI
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
 
type MyArrow b c = IOStateArrow (Set.Set String) b c
 
main = do [url] <- getArgs
          lines <- runUrl url
          mapM_ putStrLn lines

execArgs url = do
	lines <- runUrl url
	mapM_ putStrLn lines
 
split = arr (\x -> (x,x))
 
{- runUrl takes a seed URL and starts spidering from there, returning a
 - list of validation or other errors.
 -}
runUrl :: String -> IO [String]
runUrl url = runX (constA url
                   >>> setTraceLevel 0
                   >>> withOtherUserState Set.empty
                       (split
                        >>> checkUrl
                        >. unlines
                        >>> perform (getUserState
                                     >>> Set.size
                                     ^>> (trace 0 $ arr (\x -> "Checked " ++ show x ++ " urls")))
                       )
                  )
 
{- checkUrl is an arrow taking as input a pair (url, base), and producing
 - a list of errors encountered.  The url could be relative to the base.
 - This arrow will recursively check additional urls encountered in local
 - URLs.  Remote URLs are only checked for status.
 -}
checkUrl :: MyArrow (String,String) String
checkUrl = clearErrStatus
           >>>
           first normalizeUrl
           >>>
           ifA (first seenUrl)
               (fst ^>> traceString 1 ("Skipping (seen already) " ++) >>> none)
               (first markSeen
                >>>
                ifP isLocalHtml
                    validateAndSpiderUrl
                    checkUrlStatus)
 
{- readFromDocument uses the external curl program because of a file
 - descriptor leak in either Network.HTTP or HXT
 -}
validateAndSpiderUrl :: MyArrow (String, String) String
validateAndSpiderUrl = arr (\(x,y) -> (x,x))
                       >>>
                       first ( traceString 0 ("Validating " ++)
                               >>> readFromDocument [ withCurl []]
                               >>> selectLinks
                               >>> traceString 1 ("Found link: " ++)
                             )
--                     >>> arr (\(x,y) -> ((x,y),y)) >>> first expandURI
                       >>> (this &&& arr snd) >>> first expandURI
                       >>>
                       checkUrl
 
 
seenUrl :: MyArrow String String
seenUrl = split >>> second getUserState
          >>> (uncurry Set.member) `guardsP` (arr fst)
 
markSeen :: MyArrow String String
markSeen = changeUserState Set.insert
 
normalizeUrl = arrL (maybeToList . removeFragment)
    where removeFragment u = do uri <- parseURIReference u
                                return $ show uri { uriFragment = ""}
 
selectLinks :: ArrowXml a => a XmlTree String
selectLinks = deep (isElem
                    >>> hasName "a"
                    >>> getAttrValue "href"
                    >>> mkText)
              >>> getText
 
{- Note that we already expanded any relative URLs -}
isLocalHtml :: (String, String) -> Bool
isLocalHtml (url, base) = haveSameHost url base && isHtmlUrl url
    where haveSameHost a b = fromMaybe False
                                (do urlA <- parseURI a
                                    urlB <- parseURI b
                                    authA <- uriAuthority urlA
                                    authB <- uriAuthority urlB
                                    return $ uriRegName authA == uriRegName authB
                                )
          isHtmlUrl url = isSuffixOf ".html" url || isSuffixOf "/" url
 
{- Checks the status of a url and returns an error message if anything
 - other than a 200 OK response results.
 -}
checkUrlStatus :: MyArrow (String,String) String
checkUrlStatus = first (traceString 0 ("Checking status of " ++)
                        >>> arrIO (responseCode) >>> arrL (maybeToList))
                 >>> arr (\(u,b) -> u ++ " (linked from " ++ b ++ ")")
 
responseCode :: String -> IO (Maybe String)
responseCode url = case parseURIReference url of
                       Nothing -> return $ Just ("Bad URL: " ++ url)
                       Just uri -> if uriScheme uri == "http:"
                                       then catch (responseCode' uri)
                                                (\(e :: SomeException) -> return $ Just (show uri ++ ": " ++ show e))
                                       else return $ Nothing
    where responseCode' uri =
            -- HEAD would be sufficient except that some sites
            -- (like Amazon) disallow it :-(
            do result <- simpleHTTP $ Request uri GET [] ""
               return $ either (Just . ((show uri ++ ": ") ++) . show)
                               (responseMessage uri)
                               result
          responseMessage uri response =
              case classifyResponse response of
                  Success      -> Nothing
                  Error reason -> Just (show uri ++ ": " ++ reason)
                  Moved loc    -> Just (show uri ++ ": moved to " ++
                                        (fromMaybe "unknown location" loc))
 
 
-- Network.HTTP v3000.0.0 forgot to expose this type
type ResponseCode = (Int, Int, Int)
 
data HttpResponseType = Success
                      | Moved (Maybe String)   -- new location
                      | Error String  -- reason
 
classifyResponse :: Response String -> HttpResponseType
classifyResponse response =
    case rspCode response of
        (2,0,0) -> Success
        (3,0,2) -> Success   -- Found
        (3,0,7) -> Success   -- Temporary Redirect
        (3,0,1) -> Moved $ findHeader HdrLocation response
        _       -> Error $ rspReason response