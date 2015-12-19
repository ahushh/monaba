{-# LANGUAGE MultiWayIf #-}
module Handler.Thread where
 
import           Import
import qualified Data.Text          as T
import qualified Database.Esqueleto as E
import qualified Data.Map.Strict    as Map
import           Utils.File         (insertFiles)
import           Utils.YobaMarkup   (doYobaMarkup)
import           Handler.Posting
import           Handler.Captcha    (checkCaptcha)
import           Handler.EventSource (sendNewPostES)
import           Text.Blaze.Html.Renderer.String
-------------------------------------------------------------------------------------------------------------------
getJsonFromMsgR :: Text -> Handler TypedContent
getJsonFromMsgR status = do
  mMsg      <- getMessage
  msgrender <- getMessageRender
  case mMsg of
    Just msg -> selectRep $ provideJson $ object [(status, toJSON $ renderHtml msg)]
    Nothing  -> selectRep $ provideJson $ object [(status, toJSON $ msgrender MsgUnknownError)]
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
    return (post, file)
  let t = map (second catMaybes) $ Map.toList $ keyValuesToMap allPosts
  return $ (filter (\((Entity _ p1),_) -> postParent p1 == 0) t) ++ (filter (\((Entity _ p1),_) -> postParent p1 /= 0) t)

getThreadR :: Text -> Int -> Handler Html
getThreadR board thread = do
  when (thread == 0) notFound
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let permissions      = getPermissions mgroup
      hasAccessToReply = checkAccessToReply mgroup boardVal
      maxMessageLength = boardMaxMsgLength boardVal
      opModeration     = boardOpModeration boardVal
      boardTitleVal    = boardTitle        boardVal
      boardSummaryVal  = boardSummary      boardVal
      geoIpEnabled     = boardEnableGeoIp  boardVal
      showPostDate     = boardShowPostDate  boardVal
  -------------------------------------------------------------------------------------------------------
  allPosts <- selectThread board thread
  when (null allPosts) notFound
  let repliesAndFiles = drop 1 allPosts
      eOpPost         = fst $ head allPosts
      opPostFiles     = reverse $ snd $ head allPosts
      pagetitle       = makeThreadtitle eOpPost
  -------------------------------------------------------------------------------------------------------
  posterId <- getPosterId
  unless (checkHellbanned (entityVal $ eOpPost) permissions posterId) notFound
  -------------------------------------------------------------------------------------------------------
  (postFormWidget, formEnctype) <- generateFormPost $ postForm False boardVal muser
  (editFormWidget, _)           <- generateFormPost editForm

  noDeletedPosts  <- (==0) <$> runDB (count [PostBoard ==. board, PostParent ==. thread, PostDeletedByOp ==. True])
  mBanner         <- chooseBanner
  msgrender       <- getMessageRender
  AppSettings{..} <- appSettings <$> getYesod
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleReverse $ T.concat [boardTitleVal, if T.null pagetitle then "" else appTitleDelimiter, pagetitle]
    $(widgetFile "thread")
-------------------------------------------------------------------------------------------------------------------
postThreadR :: Text -> Int -> Handler Html
postThreadR board thread = do
  when (thread <= 0) notFound
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  unless (checkAccessToReply mgroup boardVal) notFound

  maybeParent <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  -------------------------------------------------------------------------------------------------------     
  let permissions      = getPermissions mgroup
      defaultName      = boardDefaultName      boardVal
      allowedTypes     = boardAllowedTypes     boardVal
      thumbSize        = boardThumbSize        boardVal
      bumpLimit        = boardBumpLimit        boardVal
      replyFile        = boardReplyFile        boardVal
      enableCaptcha    = boardEnableCaptcha    boardVal
      forcedAnon       = boardEnableForcedAnon boardVal
      threadUrl        = ThreadR board thread
      boardUrl         = BoardNoPageR board
  -------------------------------------------------------------------------------------------------------         
  ((result, _), _) <- runFormPost $ postForm False boardVal muser
  case result of
    FormFailure []                     -> trickyRedirect "error" MsgBadFormData threadUrl
    FormFailure xs                     -> trickyRedirect "error" (MsgError $ T.intercalate "; " xs) threadUrl
    FormMissing                        -> trickyRedirect "error" MsgNoFormData threadUrl
    FormSuccess (name, title, message, captcha, pswd, files, ratings, goback, nobump)
      | isNothing maybeParent                             -> trickyRedirect "error" MsgNoSuchThread        boardUrl
      | (\(Just (Entity _ p)) -> postLocked p) maybeParent -> trickyRedirect "error" MsgLockedThread        threadUrl
      | replyFile == "Disabled"&& not (noFiles files)         -> trickyRedirect "error" MsgReplyFileIsDisabled threadUrl
      | replyFile == "Required"&& noFiles files             -> trickyRedirect "error" MsgNoFile              threadUrl
      | noMessage message && noFiles files                 -> trickyRedirect "error" MsgNoFileOrText        threadUrl
      | not $ all (isFileAllowed allowedTypes) files        -> trickyRedirect "error" MsgTypeNotAllowed      threadUrl
      | otherwise                                         -> do
        ------------------------------------------------------------------------------------------------------
        setSession "message"    (maybe     "" unTextarea message)
        setSession "post-title" (fromMaybe "" title)
        ------------------------------------------------------------------------------------------------------
        posterId  <- getPosterId
        ip        <- pack <$> getIp
        now       <- liftIO getCurrentTime
        country   <- getCountry ip
        hellbanned <- (>0) <$> runDB (count [HellbanUid ==. posterId])
        ------------------------------------------------------------------------------------------------------
        checkBan ip $ \m -> trickyRedirect "error" m threadUrl
        unless (checkHellbanned (entityVal $ fromJust maybeParent) permissions posterId) notFound
        ------------------------------------------------------------------------------------------------------
        when (enableCaptcha && isNothing muser) $ checkCaptcha captcha (trickyRedirect "error" MsgWrongCaptcha threadUrl)
        ------------------------------------------------------------------------------------------------------
        checkTooFastPosting (PostParent !=. 0) ip now $ trickyRedirect "error" MsgPostingTooFast threadUrl
        ------------------------------------------------------------------------------------------------------
        messageFormatted  <- doYobaMarkup message board thread
        AppSettings{..}   <- appSettings <$> getYesod
        lastPost          <- runDB (selectFirst [PostBoard ==. board] [Desc PostLocalId])
        let nextId  = 1 + postLocalId (entityVal $ fromJust lastPost)
            newPost = Post { postBoard        = board
                           , postLocalId      = nextId
                           , postParent       = thread
                           , postParentTitle  = postTitle $ entityVal $ fromJust $ maybeParent
                           , postMessage      = messageFormatted
                           , postRawMessage   = maybe "" unTextarea message
                           , postTitle        = maybe ("" :: Text) (T.take appMaxLenOfPostTitle) title
                           , postName         =  if forcedAnon then defaultName else maybe defaultName (T.take appMaxLenOfPostName) name
                           , postDate         = now
                           , postPassword     = pswd
                           , postBumped       = Nothing
                           , postIp           = ip
                           , postCountry      = (\(code,name') -> GeoCountry code name') <$> country
                           , postLocked       = False
                           , postSticked      = False
                           , postAutosage     = False
                           , postDeleted      = False
                           , postDeletedByOp  = False
                           , postOwner        = (pack . show . userGroup . entityVal) <$> muser
                           , postHellbanned   = hellbanned
                           , postPosterId     = posterId
                           , postLastModified = Nothing                                                
                           }
        void $ insertFiles files ratings thumbSize =<< runDB (insert newPost)
        -------------------------------------------------------------------------------------------------------
        -- bump thread if necessary
        isBumpLimit <- (\x -> x >= bumpLimit && bumpLimit > 0) <$> runDB (count [PostParent ==. thread])
        unless ((fromMaybe False nobump) || isBumpLimit || postAutosage (entityVal $ fromJust maybeParent)) $ bumpThread board thread now
        -------------------------------------------------------------------------------------------------------
        case name of
          Just name' -> setSession "name" name'
          Nothing    -> deleteSession "name"
        -- everything went well, delete these values
        deleteSession "message"
        deleteSession "post-title"
        cleanBoardStats board
        unless hellbanned $ sendNewPostES board
        case goback of
          ToBoard  -> setSession "goback" "ToBoard"  >> trickyRedirect "ok" MsgPostSent (BoardNoPageR board)
          ToThread -> setSession "goback" "ToThread" >> trickyRedirect "ok" MsgPostSent threadUrl
          ToFeed   -> setSession "goback" "ToFeed"   >> trickyRedirect "ok" MsgPostSent FeedR
