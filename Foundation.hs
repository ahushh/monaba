module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.HashDB    (authHashDB, getAuthIdHashDB)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

-- Used only in Foundation.hs
import qualified Data.Text   as T
import qualified Data.Map    as Map
import qualified Network.Wai as WAI (Request(..))
import           Network.HTTP.Types (mkStatus)
import           Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import           GHC.Word (Word64)

maxFileSize :: Word64
maxFileSize = 25 -- in MB

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appSSEClients  :: TVar (Map.Map Text UTCTime)
    , appSSEChan     :: TChan (Text, Text)
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- Data types appear in routes
data ManageBoardAction = NewBoard | AllBoards | UpdateBoard
                       deriving (Show, Read, Eq)

-- i18n helpers
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
    where lastN = (head $ ((++)[0]) $ map (`mod`10) $ takeWhile (>0) $ iterate (`div`10) n)

timesRus :: Int -> String
timesRus n
  | n     `elem` [11..14] = "раз"
  | lastN `elem` [2,3,4]  = "раза"
  | otherwise             = "раз"
    where lastN = (head $ ((++)[0]) $ map (`mod`10) $ takeWhile (>0) $ iterate (`div`10) n)

plural :: Int -> String -> String -> String
plural 1 x _ = x
plural _ _ y = y

unlimitedBump :: Int -> String -> String
unlimitedBump 0 s = s
unlimitedBump n _ = show n

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    maximumContentLength _ _ = Just $ maxFileSize * (1024^(2 :: Word64))
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        (60 * 60 * 24 * 7) -- timeout in minutes, 7 days
        "config/client_session_key.aes"

    defaultLayout widget = do
        muser      <- maybeAuth
        master     <- getYesod
        mmsg       <- getMessage
        msgrender  <- getMessageRender
        boards     <- runDB $ selectList ([]::[Filter Board]) []
        categories <- (maybe [] (configBoardCategories . entityVal)) <$> runDB (selectFirst ([]::[Filter Config]) [])
        stylesheet <- flip mplus (Just $ appStylesheet $ appSettings master) <$> lookupSession "stylesheet"
        let uGroup  = (userGroup . entityVal) <$> muser

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized x _ = case x of
      AdminR{}        -> isAuthorized' [ManagePanelP]
      NewPasswordR{}  -> isAuthorized' [ManagePanelP]
      AccountR{}      -> isAuthorized' [ManagePanelP]
      StickR{}        -> isAuthorized' [ManageThreadP]
      LockR{}         -> isAuthorized' [ManageThreadP]
      AutoSageR{}     -> isAuthorized' [ManageThreadP]
      MoveThreadR{}   -> isAuthorized' [ManageThreadP] 

      BanByIpR{}      -> isAuthorized' [ManageBanP]
      ManageBoardsR{} -> isAuthorized' [ManageBoardP]
      NewBoardsR{}    -> isAuthorized' [ManageBoardP]
      UpdateBoardsR{} -> isAuthorized' [ManageBoardP]
      AllBoardsR{}    -> isAuthorized' [ManageBoardP]

      HellBanNoPageR{}  -> isAuthorized' [HellBanP]
      HellBanR{}        -> isAuthorized' [HellBanP]
      HellBanDoR{}      -> isAuthorized' [HellBanP]

      AdminSearchHBUIDR{}       -> isAuthorized' [ViewIPAndIDP, HellBanP]
      AdminSearchHBUIDNoPageR{} -> isAuthorized' [ViewIPAndIDP, HellBanP]

      DeleteBoardR{}  -> isAuthorized' [ManageBoardP]
      CleanBoardR{}   -> isAuthorized' [ManageBoardP]
      UsersR{}        -> isAuthorized' [ManageUsersP]
      ManageGroupsR{} -> isAuthorized' [ManageUsersP]
      UsersDeleteR{}  -> isAuthorized' [ManageUsersP]
      ModlogR{}       -> isAuthorized' [ViewModlogP] 
      AdminSearchIPR{}       -> isAuthorized' [ViewIPAndIDP]
      AdminSearchIPNoPageR{} -> isAuthorized' [ViewIPAndIDP]
      AdminSearchUIDR{}       -> isAuthorized' [ViewIPAndIDP]
      AdminSearchUIDNoPageR{} -> isAuthorized' [ViewIPAndIDP]

      ConfigR{}       -> isAuthorized' [ManageConfigP]
      _               -> return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    errorHandler errorResponse = do
        $(logWarn) (T.append "Error Response: " $ pack (show errorResponse))
        req <- waiRequest
        let reqwith = lookup "X-Requested-With" $ WAI.requestHeaders req
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

isAuthorized' :: [Permission] -> Handler AuthResult
isAuthorized' permissions = do
  mauth <- maybeAuth
  case mauth of
    Nothing -> return AuthenticationRequired
    Just (Entity _ user) -> do
      mGroup <- runDB $ getBy $ GroupUniqName (userGroup user)
      case mGroup of
        Nothing -> return AuthenticationRequired
        Just group -> do
          if all (`elem` groupPermissions (entityVal group)) permissions
            then return Authorized
            else return $ Unauthorized "Not permitted"

-- Path pieces
instance PathPiece ManageBoardAction where
  toPathPiece = pack . show
  fromPathPiece s =
    case reads $ unpack s of
      (i,""):_ -> Just i
      _        -> Nothing

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _   = [authHashDB (Just . UserUniqName)]
    getAuthId creds = getAuthIdHashDB AuthR (Just . UserUniqName) creds

    authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
