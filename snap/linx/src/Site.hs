{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Monoid
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Util.FileServe
import           Snap.Snaplet.PostgresqlSimple
import           Heist
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C
------------------------------------------------------------------------------
import           Application
import           Model


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

createNewProject :: Handler App App ()
createNewProject = do 
    title <- getPostParam "title"
    description <- getPostParam "description"
    newProject <- execute "INSERT INTO projects VALUES (?,?)" (title,description)
    redirect "/"


linksForUser :: Int -> Handler App App [Link]
linksForUser uid = do 
    links <- query "select l.id, url, title, u.login, l.user_id\n\
                    \from link l \n\
                    \inner join link_queue q on q.link_id = l.id\n\
                    \inner join snap_auth_user u on u.uid = l.user_id\n\
                    \where q.user_id = ? and l.id = q.link_id"
                    (Only uid)
    return links

linksForUser' :: Handler App App [Link]
linksForUser' = do
    user <- with auth currentUser
    case user of 
        Just u -> linksForUser $ read $ T.unpack $ getId u
        Nothing -> return []
  where getId u = maybe "-1" unUid $ userId u
        

needsAuth :: Handler App (AuthManager App) () -> Handler App App ()
needsAuth x = with auth $ requireUser auth (redirect "/") x


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/project/new", method POST createNewProject)
         , ("/links", linkHandler)
         , ("",          serveDirectory "static")
         ]



------------------------------------------------------------------------------
-- | Splices 

linkHandler :: Handler App App ()
linkHandler = needsAuth $ cRender "links"

{-
links :: [Link]
links = [
    Link "http://www.google.com" "Google" "Pavel"
  , Link "http://www.yahoo.com" "Yahoo!" "Alex"
  , Link "http://www.twitter.com" "Twitter" "Olga"
  ]
  -}

allCompiledSplices :: Splices (C.Splice AppHandler)
allCompiledSplices = mconcat [ allLinksSplices ]

allLinksSplices :: Splices (C.Splice AppHandler)
allLinksSplices = "allLinks" ## (renderLinks linksRuntime)

-- | Can get links from database 
linksRuntime :: RuntimeSplice (AppHandler) [Link]
linksRuntime = lift $ linksForUser' --query_ "select url, title, user_id from link"

linksRuntime' = do
    user <- currentUser
    return $ user

renderLinks :: Monad n => RuntimeSplice n [Link] -> C.Splice n
renderLinks = C.manyWithSplices C.runChildren splicesFromLink

splicesFromLink :: Monad n => Splices (RuntimeSplice n Link -> C.Splice n)
splicesFromLink = mapS (C.pureSplice . C.textSplice) $ do
    "linkUrl"       ## linkUrl
    "linkTitle"     ## linkTitle
    "linkAuthor"    ## (T.pack . show . linkAuthor)


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addConfig h $ mempty { hcCompiledSplices = allCompiledSplices }
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    p <- nestSnaplet "pg" pg pgsInit
    a <- nestSnaplet "auth" auth $
           initPostgresAuth sess p
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a p 
