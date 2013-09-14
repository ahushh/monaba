{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
import qualified Database.Persist
-- import Database.Persist.Sql (runMigrationUnsafe)
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager, def)
import Control.Monad.Logger (runLoggingT)
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)
import Control.Concurrent.Chan (newChan)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Help
import Handler.Thread
import Handler.Board
import Handler.Edit
import Handler.Live
import Handler.Delete
import Handler.Admin
import Handler.Admin.Ban
import Handler.Admin.Hellban
import Handler.Admin.Board
import Handler.Admin.Config
import Handler.Admin.Group
import Handler.Admin.User
import Handler.Admin.Search
import Handler.Admin.Modlog
import Handler.Api
import Handler.Captcha
import Handler.Settings

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = Logger $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    return $ logWare app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/mysql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)
    logger <- mkLogger True stdout
    chan <- newChan
    let foundation = App conf s p manager dbconf logger chan

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        -- (Database.Persist.runPool dbconf (runMigrationUnsafe migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
