module Handler.Posting where

import           Import
import           Handler.Admin.Ban
import qualified Data.Text as T
import           System.Directory (doesDirectoryExist, createDirectory, getDirectoryContents)
import           System.FilePath ((</>))
import           Data.Text.Encoding (encodeUtf8)
import           Text.Regex.PCRE.Heavy
--import           Data.List (isInfixOf)
--import           Data.Either
-------------------------------------------------------------------------------------------------------------------
-- This file contains some common forms and helpers for Thread.hs, Board.hs and Edit.hs
-------------------------------------------------------------------------------------------------------------------
data GoBackTo = ToThread | ToBoard | ToFeed
    deriving (Show, Read, Eq, Enum, Bounded)
-------------------------------------------------------------------------------------------------------------------
-- Forms
-------------------------------------------------------------------------------------------------------------------
postForm :: Bool  -> -- ^ Is a new thread
           Board -> -- ^ Board value
           Maybe (Entity User) -> -- ^ User
           Html                -> -- ^ Extra token
           MForm Handler (FormResult ( Maybe Text     -- ^ Poster name
                                     , Maybe Text     -- ^ Thread subject
                                     , Maybe Textarea -- ^ Message
                                     , Maybe Text     -- ^ Captcha
                                     , Text           -- ^ Password
                                     , [FormResult (Maybe FileInfo)] -- ^ Files
                                     , [FormResult Censorship]       -- ^ Censorship ratings
                                     , GoBackTo       -- ^ Go back to
                                     , Maybe Bool     -- ^ No bump
                                     , Maybe Text     -- ^ Destination post ID
                                     )
                         , Maybe (PageContent (Route App)) -> Widget)
postForm isNewThread boardVal muser extra = do
  lastName    <- lookupSession "name"
  lastGoback  <- lookupSession "goback"
  lastMessage <- lookupSession "message"
  lastTitle   <- lookupSession "post-title"
  deleteSession "message"
  deleteSession "post-title"
  msgrender   <- getMessageRender
  AppSettings{..} <- appSettings <$> getYesod

  let maxMessageLength = boardMaxMsgLength  boardVal
      numberFiles      = boardNumberFiles   boardVal
      enableCaptcha    = boardEnableCaptcha boardVal
      forcedAnon       = boardEnableForcedAnon boardVal
      myMessageField = checkBool (not . tooLongMessage maxMessageLength)
                                 (MsgTooLongMessage maxMessageLength )
                                 textareaField
      urls :: [(Text, GoBackTo)]
      urls = [(msgrender MsgToThread, ToThread), (msgrender MsgToBoard, ToBoard), (msgrender MsgToFeed, ToFeed)]
      ratings :: [(Text, Censorship)]
      ratings = map (pack . show &&& id) [minBound..maxBound]
      ratingInput  lbl = lbl { fsAttrs = [("class","rating-input"),("data-used","0")] }
      passInput    lbl = lbl { fsAttrs = [("autocomplete","off")] }
      captchaInput lbl = lbl { fsAttrs = [("class", "captcha-input"),("placeholder",msgrender MsgCaptcha)] }
      msgInput     lbl = lbl { fsAttrs = [("placeholder",msgrender MsgMessage)] }
      nameInput    lbl = lbl { fsAttrs = [("autocomplete","off"),("maxlength",tshow appMaxLenOfPostName ),("placeholder",msgrender MsgName)] }
      gobackInput  lbl = lbl { fsAttrs = [("class","goback-input")] }
      destInput lbl = lbl { fsAttrs = [("class","dest-input")] }
      subjectInput lbl = lbl { fsAttrs = [("class","subject-input"),("autocomplete","off"),("maxlength",tshow appMaxLenOfPostTitle),
                                          if isNewThread then ("placeholder",msgrender MsgThreadSubject) else ("placeholder",msgrender MsgPostSubject)]++
                                         [("required","required") | boardRequiredThreadTitle boardVal && isNewThread] }
  ----------------------------------------------------------------------------------------------------------------
  (nameRes     , nameView    ) <- mopt textField              (nameInput    "") (Just              <$> lastName)
  (subjectRes  , subjectView ) <- mopt textField              (subjectInput "") (Just              <$> lastTitle)
  (messageRes  , messageView ) <- mopt myMessageField         (msgInput     "") ((Just . Textarea) <$> lastMessage)
  (passwordRes , passwordView) <- mreq passwordField          (passInput    "") Nothing
  (captchaRes  , captchaView ) <- mopt textField              (captchaInput "") Nothing
  (gobackRes   , gobackView  ) <- mreq (selectFieldList urls) (gobackInput  "") (Just $ maybe ToBoard (\x -> tread x :: GoBackTo) lastGoback)
  (nobumpRes   , nobumpView  ) <- mopt checkBoxField                        ""  Nothing
  (fileresults , fileviews   ) <- unzip <$> forM ([1..numberFiles] :: [Int]) (\_ -> mopt fileField "File" Nothing)
  (ratingresults, ratingviews) <- unzip <$> forM ([1..numberFiles] :: [Int]) (\_ -> mreq (selectFieldList ratings) (ratingInput "") Nothing)
  (destRes     , destView    ) <- mopt hiddenField            (destInput "") Nothing
  let result = (,,,,,,,,,) <$>   nameRes <*> subjectRes <*> messageRes <*> captchaRes <*> passwordRes <*>
               FormSuccess fileresults <*> FormSuccess ratingresults <*> gobackRes  <*> nobumpRes <*> destRes
      widget captchaImg = $(widgetFile "post-form")
  return (result, widget)
-------------------------------------------------------------------------------------------------------------------
editForm :: [Permission] -> Html -> MForm Handler (FormResult (Textarea, Text, Int, Maybe Bool), Widget)
editForm permissions extra = do
  (postIdRes  , postIdView  ) <- mreq intField      "" Nothing
  (messageRes , messageView ) <- mreq textareaField "" Nothing
  (passwordRes, passwordView) <- mreq passwordField "" Nothing  
  (shadowRes  , shadowView  ) <- mopt checkBoxField "" Nothing 
  -- msgrender <- getMessageRender
  let result = (,,,) <$> messageRes <*> passwordRes <*> postIdRes <*> shadowRes
      widget = $(widgetFile "edit-form")
  return (result, widget)
-------------------------------------------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------------------------------------------
takeBanner :: Text -> Handler (Maybe (String, String))
takeBanner b = do
  let board = unpack b
  AppSettings{..} <- appSettings <$> getYesod  
  liftIO $ unlessM (doesDirectoryExist (appStaticDir </> "banners")) $ createDirectory (appStaticDir </> "banners")
  dirExists <- liftIO $ doesDirectoryExist (appStaticDir </> "banners" </> board)
  if dirExists
    then do
      banners <- filter (\b'->b'/="."&&b'/="..") <$> liftIO (getDirectoryContents (appStaticDir </> "banners" </> board))
      mBanner <- liftIO $ pick banners
      case mBanner of
        Just banner -> return $ Just ("/" ++ appStaticDir </> "banners" </> board </> banner, "/" ++ board)
        Nothing -> return Nothing
    else return Nothing

randomBanner :: Handler (Maybe (String, String))
randomBanner = do
  AppSettings{..} <- appSettings <$> getYesod
  liftIO $ do
    unlessM (doesDirectoryExist (appStaticDir </> "banners")) $ createDirectory (appStaticDir </> "banners")
    boards   <- filter (\b->b/="."&&b/="..") <$> getDirectoryContents (appStaticDir </> "banners")
    mBoard   <- pick boards
    case mBoard of
     Just board -> do
       banners <- filter (\b->b/="."&&b/="..") <$> getDirectoryContents (appStaticDir </> "banners" </> board)
       mBanner <- pick banners
       case mBanner of
        Just banner -> return $ Just ("/" ++ appStaticDir </> "banners" </> board </> banner, "/" ++ board)
        Nothing -> return Nothing
     Nothing -> return Nothing

checkBan :: IP -> Text -> (Either AppMessage Text -> HandlerT App IO ()) -> HandlerT App IO ()
checkBan ip board redirectSomewhere = do
  bans <- runDB $ selectList [BanIpBegin <=. ip, BanIpEnd >=. ip] [Desc BanId]
  let bans' = flip filter bans $ \(Entity _ b) -> board `elem` banBoards b
      mBan  = if (0 == length bans') then Nothing else Just (head bans')
  msgrender <- getMessageRender  
  timeZone  <- getTimeZone
  case mBan of
   Just eBan@(Entity _ ban) -> unlessM (isBanExpired eBan) $ do
     let m = Left $ MsgYouAreBanned (banReason ban) (maybe (msgrender MsgNeverExpires) (pack . myFormatTime timeZone) (banExpires ban))
     redirectSomewhere m
   Nothing -> return ()

checkTooFastPosting :: Filter Post -> Text -> UTCTime -> HandlerT App IO () -> HandlerT App IO ()
checkTooFastPosting cond ip now redirectSomewhere = do
  lastPost <- runDB $ selectFirst [PostIp ==. ip, cond] [Desc PostDate]
  case lastPost of
   Just (Entity _ post) -> do
     let diff = ceiling ((realToFrac $ diffUTCTime now (postDate post)) :: Double)
     whenM ((>diff) <$> getConfig configReplyDelay) redirectSomewhere
   Nothing             -> return ()


checkWordfilter :: Maybe Textarea -> Text -> (Either AppMessage Text -> HandlerT App IO ()) -> Handler ()
checkWordfilter Nothing _ _ = return ()
checkWordfilter (Just (Textarea msg)) board redirectSomewhere = do
  posterId <- getPosterId
  ip  <- pack <$> getIp
  bs  <- runDB $ selectList ([]::[Filter Wordfilter]) []
  forM_ bs $ \(Entity _ b) -> do
       let regex = compileM (encodeUtf8 $ wordfilterData b) []
       when (maybe True ((==)board) (wordfilterBoard b) && checkText msg b regex) $ do
         let as = wordfilterAction b
             m  = wordfilterActionMsg b
         when (WordfilterBan `elem` as) $ do
           void $ addBan (tread ip) (tread ip) m [] Nothing
         when (WordfilterHB `elem` as) $ do
           hellbanned <- (>0) <$> runDB (count [HellbanUid ==. posterId])
           when (not hellbanned) $ 
             void $ runDB $ insert $ Hellban { hellbanUid = posterId, hellbanIp = ip }
         when (WordfilterHBHide `elem` as) $ do
           setSession "hide-this-post" "True"
         when (elem WordfilterReplace as && isJust (wordfilterReplacement b)) $ do
           let replacement = fromJust $ wordfilterReplacement b
           case  wordfilterDataType b of
             WordfilterWords ->
               let newMsg = foldr (\x m -> T.replace x replacement m) msg (T.words $ wordfilterData b)
                 in setSession "filtered-message" newMsg
             WordfilterExactMatch -> let newMsg = T.replace (wordfilterData b) replacement msg
                                      in setSession "filtered-message" newMsg
             WordfilterRegex      -> case regex of
                                      Left _    -> return ()
                                      Right reg -> let newMsg = gsub reg replacement msg
                                                    in setSession "filtered-message" newMsg
         when (WordfilterDeny `elem` as) $ 
           redirectSomewhere $ Right m
  where checkText msg' b regex = case wordfilterDataType b of
                                  WordfilterWords      -> or (map (\m -> T.isInfixOf m msg') (T.words $ wordfilterData b))
                                  WordfilterExactMatch -> T.isInfixOf (wordfilterData b) msg'
                                  WordfilterRegex      -> case regex of
                                                           Right r -> encodeUtf8 msg' =~ r
                                                           Left _  -> False
isFileAllowed :: [String] -> FormResult (Maybe FileInfo) -> Bool
isFileAllowed allowedTypes (FormSuccess (Just x)) = fileExt x `elem` allowedTypes
isFileAllowed _            _                      = True

noMessage :: Maybe Textarea -> Bool
noMessage message = maybe True (T.null . T.filter (`notElem`(" \r\n\t"::String)) . unTextarea) message

noFiles :: forall a. [FormResult (Maybe a)] -> Bool
noFiles files = all (\(FormSuccess f) -> isNothing f) files

tooLongMessage :: Int -> Textarea -> Bool
tooLongMessage maxLen message = maxLen <= T.length (unTextarea message)
-------------------------------------------------------------------------------------------------------------------
bumpThread :: Text    -> -- ^ Board name
             Int     -> -- ^ Thread internal ID
             UTCTime -> -- ^ Up the thread to this time
             Handler ()
bumpThread board thread now = do
  maybeThread <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybeThread of
    Just (Entity thrId _) -> runDB $ update thrId [PostBumped =. Just now]
    _                     -> error "pattern matching failed at bumpThread"
-------------------------------------------------------------------------------------------------------------------
makeThreadtitle :: Entity Post -> Text
makeThreadtitle ePost =
  let maxLen = 60
      pt     = postTitle $ entityVal ePost
      pm     = stripTags $ unTextarea $ postMessage $ entityVal ePost
      pagetitle | not $ T.null pt                                 = pt
                | not $ T.null $ T.filter (`notElem`(" \r\n\t"::String)) pm = if T.length pm > maxLen
                                                                              then flip T.append "…" $ T.take maxLen pm
                                                                              else pm
                | otherwise                                              = ""
  in pagetitle
-------------------------------------------------------------------------------------------------------------------
-- | Check if ban expired
isBanExpired :: Entity Ban -> Handler Bool
isBanExpired (Entity banId ban) = do
  case banExpires ban of
    Nothing   -> return False
    Just t    -> do
      now <- liftIO getCurrentTime
      if now > t
        then runDB (delete banId) >> return True
        else return False
