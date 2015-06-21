{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Settings where
 
import           Import
import qualified Data.Text  as T
import           Data.List (sort)
import           System.FilePath ((</>))
import           System.Directory (getDirectoryContents)
import           Data.Foldable as Foldable (forM_)
import           Handler.Posting (trickyRedirect)
-------------------------------------------------------------------------------------------------------------------
settingsForm :: [(Text,Text)] -> -- ^ All boards
               [Text] -> -- ^ Ignored boards
               Html   -> -- ^ Extra token
               MForm Handler (FormResult (Int, Text, Maybe Text, Maybe [Text]), Widget)
settingsForm allBoards ignoredBoards extra = do
  AppSettings{..} <- appSettings <$> getYesod

  let stylesheetsPath = appStaticDir </> "stylesheets"
  stylesheets <- liftIO ((map ((pack &&& pack). takeWhile ((/=)'.')) . filter (\f -> f/="."&&f/="..") . sort) <$> getDirectoryContents stylesheetsPath)

  oldTimeZone <- lookupSession "timezone"
  oldStyle    <- lookupSession "stylesheet"
  (timezoneRes , timezoneView) <- mreq (selectFieldList timezones  ) "" (Just $ maybe appTimezone (read . unpack) oldTimeZone)
  (styleRes    , styleView   ) <- mreq (selectFieldList stylesheets) "" (Just $ fromMaybe appStylesheet oldStyle)
  (langRes     , langView    ) <- mopt (selectFieldList langs      ) "" Nothing
  (boardResults, boardViews  ) <- mopt (multiSelectFieldList allBoards) "" (Just $ Just ignoredBoards)
  let result = (,,,) <$> timezoneRes <*> styleRes <*> langRes <*> boardResults
      widget = $(widgetFile "settings-form")
  return (result, widget)

postSettingsR :: Handler TypedContent
postSettingsR = do
  allBoards <- map ((boardTitle &&& boardName) . entityVal) <$> runDB (selectList ([]::[Filter Board]) [])
  ignoredBoards <- getFeedBoards
  ((result, _), _) <- runFormPost $ settingsForm  allBoards ignoredBoards
  case result of
    FormFailure []                  -> trickyRedirect "error" MsgBadFormData SettingsR
    FormFailure xs                  -> trickyRedirect "error" (MsgError $ T.intercalate "; " xs) SettingsR
    FormMissing                     -> trickyRedirect "error" MsgNoFormData  SettingsR
    FormSuccess (timezone, stylesheet, lang, boards) -> do
      setSession "timezone"   $ tshow timezone
      setSession "stylesheet" stylesheet
      setSession "feed-ignore-boards" $ tshow $ fromMaybe [] boards
      Foldable.forM_ lang setLanguage
      trickyRedirect "ok" MsgApplied SettingsR

getSettingsR :: Handler Html
getSettingsR = do
  allBoards       <- map ((boardTitle &&& boardName) . entityVal) <$> runDB (selectList ([]::[Filter Board]) [])
  ignoredBoards   <- getFeedBoards
  (formWidget, formEnctype) <- generateFormPost $ settingsForm allBoards ignoredBoards
  hiddenThreads   <- getAllHiddenThreads
  msgrender       <- getMessageRender
  defaultLayout $ do
    defaultTitleMsg MsgSettings
    $(widgetFile "settings")

-------------------------------------------------------------------------------------------------------------------
langs :: [(Text, Text)]
langs = [("English", "en"), ("Русский","ru")]

timezones :: [(Text, Int)]
timezones = [("[UTC -11:00] Pacific/Midway",-39600)
            ,("[UTC -10:00] Pacific/Honolulu",-36000)
            ,("[UTC -09:30] Pacific/Marquesas",-34200)
            ,("[UTC -09:00] Pacific/Gambier",-32400)
            ,("[UTC -08:00] America/Anchorage",-28800)
            ,("[UTC -07:00] America/Los_Angeles",-25200)
            ,("[UTC -06:00] America/Costa_Rica",-21600)
            ,("[UTC -06:00] Pacific/Easter",-21600)
            ,("[UTC -05:00] America/Mexico_City",-18000)
            ,("[UTC -05:00] America/Panama",-18000)
            ,("[UTC -04:30] America/Caracas",-16200)
            ,("[UTC -04:00] America/New_York",-14400)
            ,("[UTC -03:00] America/Araguaina",-10800)
            ,("[UTC -03:00] Atlantic/Stanley",-10800)
            ,("[UTC -02:30] America/St_Johns",-9000)
            ,("[UTC -02:00] America/Noronha",-7200)
            ,("[UTC -02:00] Atlantic/South_Georgia",-7200)
            ,("[UTC -01:00] Atlantic/Cape_Verde",-3600)
            ,("[UTC -00:00] Africa/Dakar",0)
            ,("[UTC -00:00] America/Danmarkshavn",0)
            ,("[UTC -00:00] Atlantic/St_Helena",0)
            ,("[UTC -00:00] UTC",0)
            ,("[UTC +01:00] Africa/Tunis",3600)
            ,("[UTC +01:00] Atlantic/Canary",3600)
            ,("[UTC +01:00] Europe/London",3600)
            ,("[UTC +02:00] Africa/Cairo",7200)
            ,("[UTC +02:00] Europe/Berlin",7200)
            ,("[UTC +02:00] Europe/Madrid",7200)
            ,("[UTC +02:00] Europe/Paris",7200)
            ,("[UTC +02:00] Europe/Rome",7200)
            ,("[UTC +03:00] Asia/Damascus",10800)
            ,("[UTC +03:00] Asia/Jerusalem",10800)
            ,("[UTC +03:00] Europe/Kaliningrad",10800)
            ,("[UTC +03:00] Europe/Kiev",10800)
            ,("[UTC +03:00] Europe/Riga",10800)
            ,("[UTC +03:00] Indian/Comoro",10800)
            ,("[UTC +04:00] Europe/Moscow",14400)
            ,("[UTC +04:00] Asia/Tbilisi",14400)
            ,("[UTC +04:00] Indian/Mahe",14400)
            ,("[UTC +04:30] Asia/Kabul",16200)
            ,("[UTC +05:00] Asia/Baku",18000)
            ,("[UTC +06:00] Asia/Yekaterinburg",21600)
            ,("[UTC +06:00] Indian/Chagos",21600)
            ,("[UTC +07:00] Asia/Bangkok",25200)
            ,("[UTC +07:00] Asia/Omsk",25200)
            ,("[UTC +08:00] Asia/Hong_Kong",28800)
            ,("[UTC +08:00] Australia/Perth",28800)
            ,("[UTC +09:00] Asia/Tokyo",32400)
            ,("[UTC +09:00] Pacific/Palau",32400)
            ,("[UTC +09:30] Australia/Darwin",34200)
            ,("[UTC +10:00] Asia/Yakutsk",36000)
            ,("[UTC +10:00] Australia/Sydney",36000)
            ,("[UTC +10:00] Pacific/Guam",36000)
            ,("[UTC +10:30] Australia/Lord_Howe",37800)
            ,("[UTC +11:00] Asia/Vladivostok",39600)
            ,("[UTC +11:00] Pacific/Efate",39600)
            ,("[UTC +12:00] Asia/Kamchatka",43200)
            ,("[UTC +12:00] Pacific/Auckland",43200)
            ,("[UTC +12:45] Pacific/Chatham",45900)
            ,("[UTC +13:00] Pacific/Tongatapu",46800)
            ,("[UTC +14:00] Pacific/Kiritimati",50400)
            ]
