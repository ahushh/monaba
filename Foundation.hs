{-# LANGUAGE RankNTypes #-}
module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.HashDB (authHashDB, getAuthIdHashDB)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlBackend)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import ModelTypes
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)

import Data.Text (Text)
import qualified Data.Text as T

import GHC.Word (Word64)

import Network.HTTP.Types (mkStatus)
import Network.Wai (Request(..))
import Control.Monad (when)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust, isNothing, isJust)
import Text.Blaze.Html as Import (preEscapedToHtml)

import Data.List (sortBy)
import Control.Monad (mplus)

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
    }

instance HasHttpManager App where
    getHttpManager = httpManager

---------------------------------------------------------------------------------------------------------
-- Data types appear in models
---------------------------------------------------------------------------------------------------------
data ManageBoardAction = NewBoard | AllBoards | UpdateBoard
                       deriving (Show, Read, Eq)
---------------------------------------------------------------------------------------------------------
-- Template helpers
---------------------------------------------------------------------------------------------------------
widgetHelperFilterBoards :: [Entity Board] -> Text -> Maybe Text -> [Entity Board]
widgetHelperFilterBoards boards category group = filter p boards
  where p (Entity _ b)  = notHidden b && checkCategory b && checkAccess b
        notHidden     b = not $ boardHidden b
        checkCategory b | T.null category = isNothing $ boardCategory b
                        | otherwise       = Just category == boardCategory b
        checkAccess   b = isNothing (boardViewAccess b) || (isJust group && elem (fromJust group) (fromJust $ boardViewAccess b))

sortBoards :: [Entity Board] -> [Entity Board]
sortBoards bs = sortBy c bs
  where c a b = let a' = entityVal a
                    b' = entityVal b
                    n1 = boardIndex a'
                    n2 = boardIndex b'
                in if n1 == n2
                   then (boardName $ entityVal a) `compare` (boardName $ entityVal b)
                   else n1 `compare` n2
---------------------------------------------------------------------------------------------------------
-- i18n helpers
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

-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.

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

        defaultStylesheet <- extraStylesheet <$> getExtra
        stylesheet        <- flip mplus (Just defaultStylesheet) <$> lookupSession "stylesheet"

        mgroup  <- case muser of
          Just (Entity _ u) -> runDB $ getBy $ GroupUniqName $ userGroup u
          _                 -> return Nothing
        let group  = (groupName . entityVal) <$> mgroup
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            -- addScriptRemote "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"
            -- $(combineStylesheets 'StaticR
            --     [
            --     -- css_normalize_css
            --     -- , css_bootstrap_css
            --     ])
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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
      _               -> return Authorized

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

isAuthorized' permissions = do
  mauth <- maybeAuth
  case mauth of
    Nothing -> return AuthenticationRequired
    Just (Entity _ user) -> do
      group <- runDB $ getBy $ GroupUniqName (userGroup user)
      if all (`elem` groupPermissions (entityVal $ fromJust group)) permissions
        then return Authorized
        else return $ Unauthorized "Not permitted"

instance YesodAuthPersist App
---------------------------------------------------------------------------------------------------------
-- Path pieces
---------------------------------------------------------------------------------------------------------
instance PathPiece ManageBoardAction where
  toPathPiece = T.pack . show
  fromPathPiece s =
    case reads $ T.unpack s of
      (i,""):_ -> Just i
      _        -> Nothing
---------------------------------------------------------------------------------------------------------
-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    
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
