{-# LANGUAGE TupleSections, OverloadedStrings, MultiWayIf, BangPatterns #-}
module Handler.Thread where
 
import           Import
import           Yesod.Auth
import           Prelude             (head)
import qualified Data.Text           as T
import qualified Database.Esqueleto  as E
import qualified Data.Map.Strict     as MapS
import qualified Text.Blaze.Html.Renderer.String as RHS
import           Utils.YobaMarkup    (doYobaMarkup)
import           Handler.Captcha     (checkCaptcha, recordCaptcha, getCaptchaInfo, updateAdaptiveCaptcha)
import           Handler.Posting
import           Handler.EventSource (sendPost)
-------------------------------------------------------------------------------------------------------------------
-- Ajax hack
-------------------------------------------------------------------------------------------------------------------
getJsonFromMsgR :: Text -> Handler TypedContent
getJsonFromMsgR status = do
  msg <- getMessage
  selectRep $
    provideJson $ object [(status, toJSON $ RHS.renderHtml $ fromJust msg)]
-------------------------------------------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------------------------------------------
selectThread :: Text -> Int -> Handler [(Entity Post, [Entity Attachedfile])]
selectThread board thread = do
  allPosts <- runDB $ E.select $ E.from $ \(post `E.LeftOuterJoin` file) -> do
    E.on $ (E.just (post E.^. PostId)) E.==. (file E.?. AttachedfileParentId)
    E.where_ ((post E.^. PostBoard       ) E.==. (E.val board ) E.&&.
              (post E.^. PostDeletedByOp ) E.==. (E.val False ) E.&&.
              (post E.^. PostDeleted     ) E.==. (E.val False ) E.&&.
             ((post E.^. PostParent      ) E.==. (E.val thread) E.||.
             ((post E.^. PostParent      ) E.==. (E.val 0     ) E.&&. (post E.^. PostLocalId) E.==. (E.val thread))))
    E.orderBy [E.asc (post E.^. PostId)]
    return (post, file)
  return $ map (second catMaybes) $ MapS.toList $ keyValuesToMap allPosts

getThreadR :: Text -> Int -> Handler Html
getThreadR board thread = do
  when (thread == 0) notFound
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let permissions      = getPermissions mgroup
      hasAccessToReply = checkAccessToReply mgroup boardVal
      numberFiles      = boardNumberFiles     boardVal
      maxMessageLength = boardMaxMsgLength    boardVal
      enableCaptcha    = boardEnableCaptcha   boardVal
      opModeration     = boardOpModeration    boardVal
      boardDesc        = boardDescription     boardVal
      boardLongDesc    = boardLongDescription boardVal
      geoIpEnabled     = boardEnableGeoIp     boardVal
      sourceEventName  = T.concat [board, "-", showText thread]
  -------------------------------------------------------------------------------------------------------
  allPosts <- selectThread board thread
  when (null allPosts) notFound
  let repliesAndFiles = drop 1 allPosts
      eOpPost         = fst $ head allPosts
      opPostFiles     = snd $ head allPosts
      pagetitle       = makeThreadtitle eOpPost
  -------------------------------------------------------------------------------------------------------
  posterId <- getPosterId
  unless (checkHellbanned eOpPost permissions posterId) notFound
  -------------------------------------------------------------------------------------------------------
  geoIps <- getCountries (if geoIpEnabled then allPosts else [])
  -------------------------------------------------------------------------------------------------------
  acaptcha <- lookupSession "acaptcha"
  when (isNothing acaptcha && enableCaptcha && isNothing muser) $ recordCaptcha =<< getConfig configCaptchaLength
  ------------------------------------------------------------------------------------------------------- 
  (formWidget , formEnctype) <- generateFormPost $ postForm boardVal
  (formWidget',           _) <- generateFormPost $ editForm permissions
  nameOfTheBoard   <- extraSiteName <$> getExtra
  maybeCaptchaInfo <- getCaptchaInfo
  msgrender        <- getMessageRender
  timeZone         <- getTimeZone
  boards           <- runDB $ selectList ([]::[Filter Board]) []
  noDeletedPosts   <- (==0) <$> runDB (count [PostBoard ==. board, PostParent ==. thread, PostDeletedByOp ==. True])
  rating           <- getCensorshipRating
  displaySage      <- getConfig configDisplaySage
  maxLenOfFileName <- extraMaxLenOfFileName <$> getExtra
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, titleDelimiter, boardDesc, if T.null pagetitle then "" else titleDelimiter, pagetitle]
    $(widgetFile "thread")
-------------------------------------------------------------------------------------------------------------------
-- Handlers
-------------------------------------------------------------------------------------------------------------------
postThreadR :: Text -> Int -> Handler Html
postThreadR board thread = do
  when (thread == 0) notFound
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  unless (checkAccessToReply mgroup boardVal) notFound

  maybeParent <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  -------------------------------------------------------------------------------------------------------     
  let defaultName      = boardDefaultName   boardVal
      allowedTypes     = boardAllowedTypes  boardVal
      thumbSize        = boardThumbSize     boardVal
      bumpLimit        = boardBumpLimit     boardVal
      enableCaptcha    = boardEnableCaptcha boardVal
      replyFile        = boardReplyFile     boardVal
      threadUrl        = ThreadR board thread
  -------------------------------------------------------------------------------------------------------         
  ((result, _), _) <- runFormPost $ postForm boardVal
  case result of
    FormFailure []                     -> trickyRedirect "error" MsgBadFormData threadUrl
    FormFailure xs                     -> trickyRedirect "error" (MsgError $ T.intercalate "; " xs) threadUrl
    FormMissing                        -> trickyRedirect "error" MsgNoFormData  threadUrl
    FormSuccess (name, title, message, pswd, captcha, files, ratings, goback, Just nobump)
      | replyFile == "Disabled"&& not (noFiles files)         -> trickyRedirect "error" MsgReplyFileIsDisabled threadUrl
      | replyFile == "Required"&& noFiles files             -> trickyRedirect "error" MsgNoFile              threadUrl
      | (\(Just (Entity _ p)) -> postLocked p) maybeParent -> trickyRedirect "error" MsgLockedThread        threadUrl
      | noMessage message && noFiles files                 -> trickyRedirect "error" MsgNoFileOrText        threadUrl
      | not $ all (isFileAllowed allowedTypes) files        -> trickyRedirect "error" MsgTypeNotAllowed      threadUrl
      | otherwise                                         -> do
        -- save form values in case something goes wrong
        setSession "message"    (maybe     "" unTextarea message)
        setSession "post-title" (fromMaybe "" title)
        ip        <- pack <$> getIp
        -- check ban
        msgrender <- getMessageRender
        ban <- runDB $ selectFirst [BanIp ==. ip] [Desc BanId]
        when (isJust ban) $
          unlessM (isBanExpired $ fromJust ban) $ do
            let m =  MsgYouAreBanned (banReason $ entityVal $ fromJust ban)
                                     (maybe (msgrender MsgNeverExpires) (pack . myFormatTime 0) (banExpires $ entityVal $ fromJust ban))
            trickyRedirect "error" m threadUrl
        -- check captcha
        when (enableCaptcha && isNothing muser) $ do
          acaptcha  <- lookupSession "acaptcha"
          when (isNothing acaptcha) $ do
            void $ when (isNothing captcha) (trickyRedirect "error" MsgWrongCaptcha threadUrl)
            checkCaptcha (fromJust captcha) (trickyRedirect "error" MsgWrongCaptcha threadUrl)
          updateAdaptiveCaptcha acaptcha
        ------------------------------------------------------------------------------------------------------           
        now      <- liftIO getCurrentTime
        -- check too fast posting
        lastPost <- runDB $ selectFirst [PostIp ==. ip, PostParent !=. 0] [Desc PostDate] -- last reply by IP
        when (isJust lastPost) $ do
          let diff = ceiling ((realToFrac $ diffUTCTime now (postDate $ entityVal $ fromJust lastPost)) :: Double)
          whenM ((>diff) <$> getConfig configReplyDelay) $ 
            deleteSession "acaptcha" >>
            trickyRedirect "error" MsgPostingTooFast threadUrl
        ------------------------------------------------------------------------------------------------------
        posterId         <- getPosterId
        hellbanned       <- (>0) <$> runDB (count [HellbanUserId ==. posterId])
        messageFormatted <- doYobaMarkup message board thread
        lastPost'        <- runDB (selectFirst [PostBoard ==. board] [Desc PostLocalId])
        when (isNothing lastPost') $  -- reply to non-existent thread
          trickyRedirect "error" MsgNoSuchThread (BoardNoPageR board)
        maxLenOfPostTitle <- extraMaxLenOfPostTitle <$> getExtra
        maxLenOfPostName  <- extraMaxLenOfPostName  <$> getExtra
        let nextId  = 1 + postLocalId (entityVal $ fromJust lastPost')
            newPost = Post { postBoard        = board
                           , postLocalId      = nextId
                           , postParent       = thread
                           , postMessage      = messageFormatted
                           , postRawMessage   = maybe "" unTextarea message
                           , postTitle        = maybe ("" :: Text) (T.take maxLenOfPostTitle) title
                           , postName         = maybe defaultName (T.take maxLenOfPostName ) name
                           , postDate         = now
                           , postPassword     = pswd
                           , postBumped       = Nothing
                           , postIp           = ip
                           , postSage         = nobump
                           , postLocked       = False
                           , postSticked      = False
                           , postAutosage     = False
                           , postDeleted      = False
                           , postDeletedByOp  = False
                           , postOwner        = (showText . userGroup . entityVal) <$> muser
                           , postHellbanned   = hellbanned
                           , postPosterId     = posterId
                           , postLastModified = Nothing                                                
                           }
        void $ insertFiles files ratings thumbSize =<< runDB (insert newPost)
        sendPost board thread nextId hellbanned posterId
        -------------------------------------------------------------------------------------------------------
        -- bump thread if it's necessary
        isBumpLimit <- (\x -> x >= bumpLimit && bumpLimit > 0) <$> runDB (count [PostParent ==. thread])
        unless (nobump || isBumpLimit || postAutosage (entityVal $ fromJust maybeParent)) $ bumpThread board thread now
        -- remember poster name
        case name of
          Just n  -> setSession "name" n
          Nothing -> deleteSession "name"
        -- everything went well, delete these values
        deleteSession "message"
        deleteSession "post-title"
        case goback of
          ToBoard  -> setSession "goback" "ToBoard"  >> trickyRedirect "ok" MsgPostSent (BoardNoPageR board)
          ToThread -> setSession "goback" "ToThread" >> trickyRedirect "ok" MsgPostSent threadUrl
    _  -> trickyRedirect "error" MsgUnknownError threadUrl
-------------------------------------------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------------------------------------------
makeThreadtitle :: Entity Post -> Text
makeThreadtitle ePost =
  let maxLen = 60
      pt     = postTitle $ entityVal ePost
      pm     = stripTags $ unTextarea $ postMessage $ entityVal ePost
      pagetitle | not $ T.null pt                                 = pt
                | not $ T.null $ T.filter (`notElem`" \r\n\t") pm = if T.length pm > maxLen
                                                                    then flip T.append "…" $ T.take maxLen pm
                                                                    else pm
                | otherwise                                     = ""
  in pagetitle
