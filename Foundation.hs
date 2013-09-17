{-# LANGUAGE RankNTypes #-}
module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.HashDB (authHashDB, getAuthIdHashDB)
import Yesod.Auth.Message
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT, SqlBackend)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import ModelTypes
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import System.Log.FastLogger (Logger)

import Data.Text (Text)
import qualified Data.Text as T

import GHC.Word (Word64)

import Network.HTTP.Types (mkStatus)
import Network.Wai (Request(..))
import Control.Monad (when)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust, isNothing, isJust)

import Control.Concurrent.Chan (Chan)
import Network.Wai.EventSource (ServerEvent (..))

import           Data.IORef
import           Data.Map   (Map)
import qualified Data.Map as Map

import           Data.Digest.OpenSSL.MD5 (md5sum)
import           System.Random           (randomIO)
import qualified Data.ByteString.UTF8    as B
import           Control.Applicative     (liftA2)
import           Data.Time               (getCurrentTime)
---------------------------------------------------------------------------------------------------------
data SSEClient = SSEClient { sseClientUser :: Maybe (Entity User)
                           , sseClientPermissions :: [Permission]
                           , sseClientRating :: Censorship
                           , sseClientTimeZone :: Int
                           , sseClientEvent :: Chan ServerEvent
                           }

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    , sseClients :: IORef (Map Text SSEClient)
    }
---------------------------------------------------------------------------------------------------------
-- Data types appear in models
---------------------------------------------------------------------------------------------------------
data Censorship = SFW | R15 | R18 | R18G
    deriving (Show, Read, Eq, Enum, Bounded, Ord)

data ManageBoardAction = NewBoard | AllBoards | UpdateBoard
                       deriving (Show, Read, Eq)
---------------------------------------------------------------------------------------------------------
-- Template helper
---------------------------------------------------------------------------------------------------------
widgetHelperFilterBoards :: [Entity Board] -> Text -> Maybe Text -> [Entity Board]
widgetHelperFilterBoards boards category group = filter p boards
  where p (Entity _ b)  = notHidden b && checkCategory b && checkAccess b
        notHidden     b = not $ boardHidden b
        checkCategory b | T.null category = isNothing $ boardCategory b
                        | otherwise       = Just category == boardCategory b
        checkAccess   b = isNothing (boardViewAccess b) || (isJust group && elem (fromJust group) (fromJust $ boardViewAccess b))
---------------------------------------------------------------------------------------------------------
-- I18n helpers
---------------------------------------------------------------------------------------------------------
omittedRus :: Int -> String
omittedRus n
  | n == 1               = "пост пропущен"
  | n `elem` [2..4]     = "поста пропущено"
  | n `elem` [5..19]    = "постов пропущено"
  | lastN == 0           = "постов пропущено"
  | lastN == 1           = "пост пропущен"
  | lastN `elem` [2..4] = "поста пропущено"
  | lastN `elem` [5..9] = "постов пропущено"
  | otherwise           = "постов пропущено"
    where lastN = read $ (:[]) $ last $ show n
          lastN :: Int

timesRus :: Int -> String
timesRus n
  | n     `elem` [11..14] = "раз"
  | lastN `elem` [2,3,4]  = "раза"
  | otherwise             = "раз"
    where lastN = read $ (:[]) $ last $ show n
          lastN :: Int

plural :: Int -> String -> String -> String
plural 1 x _ = x
plural _ _ y = y

unlimitedBump :: Int -> String -> String
unlimitedBump 0 s = s
unlimitedBump n _ = show n

maxFileSize :: Word64
maxFileSize = 15 -- in MB
---------------------------------------------------------------------------------------------------------
-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
--
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings
    maximumContentLength _ _ = Just $ maxFileSize * (1024^(2 :: Word64))
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (60 * 60 * 24 * 7) -- 7 days
        "config/client_session_key.aes"

    defaultLayout widget = do
        muser  <- maybeAuth
        master <- getYesod
        mmsg   <- getMessage
        msgrender  <- getMessageRender   
        boards     <- runDB $ selectList ([]::[Filter Board]) []
        categories <- configBoardCategories . entityVal . fromJust <$> runDB (selectFirst ([]::[Filter Config]) [])
        mgroup  <- case muser of
          Just (Entity _ u) -> runDB $ getBy $ GroupUniqName $ userGroup u
          _                 -> return Nothing
        let group  = (groupName . entityVal) <$> mgroup
        stylesheet <- lookupSession "stylesheet"
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addScript (StaticR js_jquery_min_js)
            addScript (StaticR js_jquery_form_js)
            addScript (StaticR js_jquery_autosize_js)            
            addScriptRemote "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"
            -- $(combineStylesheets 'StaticR
            --     [
            --     -- css_normalize_css
            --     -- , css_bootstrap_css
            --     ])
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
        -- addStaticContentExternal (\x -> Right x) genFileName Settings.staticDir (StaticR . flip StaticRoute []) -- debug
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

    isAuthorized x _ = case x of
      AdminR{}        -> isAuthorized' [ManagePanelP]
      NewPasswordR{}  -> isAuthorized' [ManagePanelP]
      AccountR{}      -> isAuthorized' [ManagePanelP]
      StickR{}        -> isAuthorized' [ManageThreadP]
      LockR{}         -> isAuthorized' [ManageThreadP]
      AutoSageR{}     -> isAuthorized' [ManageThreadP]

      BanByIpR{}      -> isAuthorized' [ManageBanP]
      ManageBoardsR{} -> isAuthorized' [ManageBoardP]
      NewBoardsR{}    -> isAuthorized' [ManageBoardP]
      UpdateBoardsR{} -> isAuthorized' [ManageBoardP]
      AllBoardsR{}    -> isAuthorized' [ManageBoardP]

      DeleteBoardR{}  -> isAuthorized' [ManageBoardP]
      CleanBoardR{}   -> isAuthorized' [ManageBoardP]
      UsersR{}        -> isAuthorized' [ManageUsersP]
      ManageGroupsR{} -> isAuthorized' [ManageUsersP]
      UsersDeleteR{}  -> isAuthorized' [ManageUsersP]

      ConfigR{}       -> isAuthorized' [ManageConfigP]
      HellBanNoPageR{}-> isAuthorized' [HellBanP]
      HellBanR{}      -> isAuthorized' [HellBanP]
      HellBanDoR{}    -> isAuthorized' [HellBanP]

      AdminSearchIPR{}       -> isAuthorized' [ViewIPAndIDP]
      AdminSearchIPNoPageR{} -> isAuthorized' [ViewIPAndIDP]
      AdminSearchUIDR{}       -> isAuthorized' [ViewIPAndIDP]
      AdminSearchUIDNoPageR{} -> isAuthorized' [ViewIPAndIDP]
      AdminSearchUIDOnlyHBR{}       -> isAuthorized' [ViewIPAndIDP, HellBanP]
      AdminSearchUIDOnlyHBNoPageR{} -> isAuthorized' [ViewIPAndIDP, HellBanP]

      ManageCensorshipR{} -> isAuthorized' [ChangeFileRatingP]
      ModlogR{}           -> isAuthorized' [ViewModlogP]
      _                   -> return Authorized

    errorHandler errorResponse = do
        $(logWarn) (T.append "Error Response: " $ T.pack (show errorResponse))
        req <- waiRequest
        let reqwith = lookup "X-Requested-With" $ requestHeaders req
            errorText NotFound               = (404, "Not Found", "Sorry, not found")
            errorText (InternalError msg)    = (400, "Bad Request", msg)
            errorText (InvalidArgs m)        = (400, "Bad Request", T.unwords m)
            errorText (PermissionDenied msg) = (403, "Forbidden", msg)
            errorText (BadMethod _)          = (405, "Method Not Allowed", "Method not supported")
            errorText NotAuthenticated       = (401, "Not authenticated", "You don't have permission to do this")
        when (maybe False (== "XMLHttpRequest") reqwith) $ do
            let (code, brief, full) = errorText errorResponse
            sendResponseStatus
                (mkStatus code brief)
                $ RepPlain $ toContent $ T.append "Error: " full
        defaultErrorHandler errorResponse

isAuthorized' :: forall master.
                 (YesodPersist master,
                  PersistUnique (YesodPersistBackend master (HandlerT master IO)),
                  YesodAuth master, AuthId master ~ KeyBackend SqlBackend User,
                  PersistMonadBackend
                  (YesodPersistBackend master (HandlerT master IO))
                  ~ SqlBackend) =>
                 [Permission] -> HandlerT master IO AuthResult
isAuthorized' permissions = do
  mauth <- maybeAuth
  case mauth of
    Nothing -> return AuthenticationRequired
    Just (Entity _ user) -> do
      group <- runDB $ getBy $ GroupUniqName (userGroup user)
      if all (`elem` groupPermissions (entityVal $ fromJust group)) permissions
        then return Authorized
        else return $ Unauthorized "Not permitted"
---------------------------------------------------------------------------------------------------------
-- Path pieces
---------------------------------------------------------------------------------------------------------
instance PathPiece ManageBoardAction where
  toPathPiece = T.pack . show
  fromPathPiece s =
    case reads $ T.unpack s of
      (i,""):_ -> Just i
      _        -> Nothing

instance PathPiece Bool where
  toPathPiece True  = "True"
  toPathPiece False = "False"
  fromPathPiece s =
    case reads $ T.unpack s of
      (i,""):_ -> Just i
      _        -> Nothing

instance PathPiece Censorship where
  toPathPiece     = T.pack . show 
  fromPathPiece s =
    case reads $ T.unpack s of
      (i,""):_ -> Just i
      _        -> Nothing
---------------------------------------------------------------------------------------------------------
-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

deleteClient' :: Handler ()
deleteClient' = do
  maybePosterId <- lookupSession "posterId"
  posterId      <- case maybePosterId of
    Just posterId -> return posterId
    Nothing       -> do
      posterId <- liftIO $ T.pack . md5sum . B.fromString <$> liftA2 (++) (show <$> (randomIO :: IO Int)) (show <$> getCurrentTime)
      setSession "posterId" posterId
      return posterId
  (\clientsRef -> liftIO $ modifyIORef clientsRef $ Map.delete posterId) =<< sseClients <$> getYesod
  
instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    
    onLogin  = setMessageI NowLoggedIn >> deleteClient'
    onLogout = deleteClient'
      
    authPlugins _   = [authHashDB (Just . UserUniqName)]
    getAuthId creds = getAuthIdHashDB AuthR (Just . UserUniqName) creds
    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
