module Handler.Board where

import           Import
import qualified Data.Text       as T
import           Handler.Common  (deletePosts)
import           Handler.Posting
import           Handler.Captcha (checkCaptcha)
import           Handler.EventSource (sendNewPostES)
import           Handler.Captcha (captchaWidget)
import           Utils.File            (insertFiles)
import           Utils.YobaMarkup      (doYobaMarkup)
import           Data.Digest.OpenSSL.MD5 (md5sum)
import qualified Data.ByteString.UTF8    as B
--------------------------------------------------------------------------------------------------------- 
getBoardNoPageR :: Text -> Handler Html
getBoardNoPageR board = getBoardR board 0

postBoardNoPageR :: Text -> Handler Html
postBoardNoPageR board = postBoardR board 0
--------------------------------------------------------------------------------------------------------- 
selectThreadsAndPreviews :: Text  -> -- ^ Board name
                           Int   -> -- ^ Page
                           Int   -> -- ^ Threads per page
                           Int   -> -- ^ Previews per thread
                           Text  -> -- ^ posterId
                           [Permission] ->
                           [Int] -> -- ^ Hidden threads
                           Handler [(  (Entity Post, [Entity Attachedfile])
                                    , [(Entity Post, [Entity Attachedfile])]
                                    , Int
                                    )]
selectThreadsAndPreviews board page threadsPerPage previewsPerThread posterId permissions hiddenThreads =
  let selectThreadsAll = selectList [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False, PostLocalId /<-. hiddenThreads]
                                    [Desc PostSticked, Desc PostBumped, LimitTo threadsPerPage, OffsetBy $ page*threadsPerPage]
      selectThreadsHB  = selectList ( [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False, PostLocalId /<-. hiddenThreads, PostHellbanned ==. False] ||.
                                      [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False, PostLocalId /<-. hiddenThreads, PostHellbanned ==. True, PostPosterId ==. posterId]
                                    ) [Desc PostSticked, Desc PostBumped, LimitTo threadsPerPage, OffsetBy $ page*threadsPerPage]
      selectThreads = if elem HellBanP permissions then selectThreadsAll else selectThreadsHB
      --------------------------------------------------------------------------------------------------
      selectFiles  pId = selectList [AttachedfileParentId ==. pId] []
      --------------------------------------------------------------------------------------------------
      selectPreviews   = if elem HellBanP permissions then selectPreviewsAll else selectPreviewsHB
      selectPreviewsHB t
        | previewsPerThread > 0 = selectList ( [PostDeletedByOp ==. False, PostBoard ==. board, PostDeleted ==. False, PostParent ==. postLocalId t, PostHellbanned ==. False] ||.
                                               [PostDeletedByOp ==. False, PostBoard ==. board, PostDeleted ==. False, PostParent ==. postLocalId t, PostHellbanned ==. True, PostPosterId ==. posterId]
                                             ) [Desc PostDate, LimitTo previewsPerThread]
        | otherwise             = return []
      selectPreviewsAll t
        | previewsPerThread > 0 = selectList [PostDeletedByOp ==. False, PostBoard ==. board, PostDeleted ==. False, PostParent ==. postLocalId t] [Desc PostDate, LimitTo previewsPerThread]
        | otherwise             = return []
      --------------------------------------------------------------------------------------------------
      countPostsAll t = [PostDeletedByOp ==. False, PostDeleted ==. False, PostBoard ==. board, PostParent ==. postLocalId t]
      countPostsHB  t = [PostDeletedByOp ==. False, PostDeleted ==. False, PostBoard ==. board, PostParent ==. postLocalId t, PostHellbanned ==. False] ||. 
                        [PostDeletedByOp ==. False, PostDeleted ==. False, PostBoard ==. board, PostParent ==. postLocalId t, PostHellbanned ==. True, PostPosterId ==. posterId]
      countPosts t = if elem HellBanP permissions then countPostsAll t else countPostsHB t
  in runDB $ selectThreads >>= mapM (\th@(Entity tId t) -> do
       threadFiles      <- selectFiles tId
       previewsAndFiles <- selectPreviews t >>= mapM (\pr -> do
         previewFiles <- selectFiles $ entityKey pr
         return (pr, previewFiles))
       postsInThread <- count (countPosts t)
       return ((th, threadFiles), reverse previewsAndFiles, postsInThread - previewsPerThread))
--------------------------------------------------------------------------------------------------------- 
getBoardR :: Text -> Int -> Handler Html
getBoardR board page = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let hasAccessToNewThread = checkAccessToNewThread mgroup boardVal
      hasAccessToReply     = checkAccessToReply mgroup boardVal
      permissions          = getPermissions mgroup
  ------------------------------------------------------------------------------------------------------- 
  numberOfThreads <- runDB $ count [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False, PostHellbanned ==. False]
  posterId        <- getPosterId
  hiddenThreads   <- map fst <$> getHiddenThreads board
  cleanBoardStats board
  -- let numberFiles       = boardNumberFiles       boardVal
  let maxMessageLength  = boardMaxMsgLength      boardVal
      threadsPerPage    = boardThreadsPerPage    boardVal
      previewsPerThread = boardPreviewsPerThread boardVal
      title             = boardTitle             boardVal
      summary           = boardSummary           boardVal
      geoIpEnabled      = boardEnableGeoIp       boardVal
      showPostDate      = boardShowPostDate      boardVal
      enablePM          = boardEnablePM          boardVal
      pages             = listPages threadsPerPage numberOfThreads
  threadsAndPreviews <- selectThreadsAndPreviews board page threadsPerPage previewsPerThread posterId permissions hiddenThreads
  ------------------------------------------------------------------------------------------------------- 
  adaptiveCaptcha <- getConfig configAdaptiveCaptcha
  pc <- lookupSession "post-count"
  let isCaptchaEnabled = boardEnableCaptcha boardVal && maybe True (\x -> tread x <= adaptiveCaptcha) pc && isNothing muser
  captchaImg <- if isCaptchaEnabled  then Just <$> widgetToPageContent captchaWidget else return Nothing
  ------------------------------------------------------------------------------------------------------- 
  (postFormWidget, formEnctype) <- generateFormPost $ postForm True boardVal muser
  (editFormWidget, _)           <- generateFormPost $ editForm permissions
  msgrender       <- getMessageRender
  AppSettings{..} <- appSettings <$> getYesod
  mBanner         <- if appRandomBanners then randomBanner else takeBanner board
  ((_, searchWidget), _) <- runFormGet $ searchForm $ Just board
  defaultLayout $ do
    setUltDestCurrent
    let p = if page > 0 then T.concat [" (", tshow page, ") "] else ""
      in defaultTitleReverse (title <> p)
    $(widgetFile "board")
    
postBoardR :: Text -> Int -> Handler Html
postBoardR board _ = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  unless (checkAccessToNewThread mgroup boardVal) notFound
  -------------------------------------------------------------------------------------------------------   
  let msgRedirect msg  = setMessageI msg >> redirect (BoardNoPageR board)
      defaultName      = boardDefaultName   boardVal
      allowedTypes     = boardAllowedTypes  boardVal
      thumbSize        = boardThumbSize     boardVal
      opFile           = boardOpFile        boardVal
      forcedAnon       = boardEnableForcedAnon boardVal
      enableCaptcha    = boardEnableCaptcha boardVal
  -------------------------------------------------------------------------------------------------------       
  ((result, _),   _) <- runFormPost $ postForm True boardVal muser
  case result of
    FormFailure []                     -> msgRedirect MsgBadFormData
    FormFailure xs                     -> msgRedirect $ MsgError $ T.intercalate "; " xs
    FormMissing                        -> msgRedirect MsgNoFormData
    FormSuccess (name, title, message, captcha, pswd, files, ratings, goback, _, _)
      | isNothing title && boardRequiredThreadTitle boardVal -> msgRedirect MsgThreadTitleIsRequired
      | opFile == "Disabled"&& not (noFiles files)      -> msgRedirect MsgOpFileIsDisabled
      | opFile == "Required"&& noFiles files          -> msgRedirect MsgNoFile
      | noMessage message  && noFiles files          -> msgRedirect MsgNoFileOrText
      | not $ all (isFileAllowed allowedTypes) files  -> msgRedirect MsgTypeNotAllowed
      | otherwise                                   -> do
        -------------------------------------------------------------------------------------------------------
        setSession "message"    (maybe     "" unTextarea message)
        setSession "post-title" (fromMaybe "" title)
        -------------------------------------------------------------------------------------------------------
        ip       <- pack <$> getIp
        now      <- liftIO getCurrentTime
        country  <- getCountry ip
        posterId <- getPosterId
        pc <- lookupSession "post-count"
        -------------------------------------------------------------------------------------------------------
        checkBan (tread ip) board $ \(Left m) -> setMessageI m >> redirect (BoardNoPageR board)
        -------------------------------------------------------------------------------------------------------
        adaptiveCaptcha <- getConfig configAdaptiveCaptcha
        when (maybe False (\x -> tread x < adaptiveCaptcha) pc && enableCaptcha && isNothing muser) $ 
          checkCaptcha captcha (setMessageI MsgWrongCaptcha >> redirect (BoardNoPageR board))
        -------------------------------------------------------------------------------------------------------
        checkTooFastPosting (PostParent ==. 0) ip now $ setMessageI MsgPostingTooFast >> redirect (BoardNoPageR board)
        ------------------------------------------------------------------------------------------------------
        checkWordfilter (Textarea <$> title) board $ \(Right m) -> setMessage (toHtml m) >> redirect (BoardNoPageR board)
        checkWordfilter message board $ \(Right m) -> setMessage (toHtml m) >> redirect (BoardNoPageR board)
        ------------------------------------------------------------------------------------------------------
        globalHB <- getConfig configGlobalHellban
        when (globalHB && maybe True ((==0).tread) pc) $
          void $ runDB $ insert $ Hellban { hellbanUid = posterId, hellbanIp = "" }
        hellbannedUID <- (>0) <$> runDB (count [HellbanUid ==. posterId])
        hellbannedIP  <- (>0) <$> runDB (count [HellbanIp ==. ip])
        ------------------------------------------------------------------------------------------------------
        nextId <- maybe 1 ((+1) . postLocalId . entityVal) <$> runDB (selectFirst [PostBoard ==. board] [Desc PostLocalId])
        newMsg <- lookupSession "filtered-message"
        messageFormatted  <- doYobaMarkup (maybe message (Just . Textarea) newMsg) board 0
        AppSettings{..}   <- appSettings <$> getYesod
        let newPost = Post { postBoard        = board
                           , postLocalId      = nextId
                           , postParent       = 0
                           , postParentTitle  = ""
                           , postMessage      = messageFormatted
                           , postRawMessage   = fromMaybe (maybe "" unTextarea message) newMsg
                           , postTitle        = maybe ("" :: Text) (T.take appMaxLenOfPostTitle) title
                           , postName         = if forcedAnon then defaultName else maybe defaultName (T.take appMaxLenOfPostName) name
                           , postDate         = now
                           , postPassword     = pack $ md5sum $ B.fromString $ unpack pswd
                           , postBumped       = Just now
                           , postIp           = ip
                           , postCountry      = (\(code,name') -> GeoCountry code name') <$> country
                           , postLocked       = False
                           , postSticked      = False
                           , postAutosage     = False
                           , postDeleted      = False
                           , postDeletedByOp  = False
                           , postOwner        = userGroup . entityVal <$> muser
                           , postOwnerUser    = userName . entityVal <$> muser
                           , postHellbanned   = hellbannedUID || hellbannedIP
                           , postPosterId     = posterId
                           , postLastModified = Nothing
                           , postLockEditing  = False
                           , postDestUID      = Nothing
                           }
        postKey <- runDB (insert newPost)
        void $ insertFiles files ratings thumbSize postKey
        hb <- lookupSession "hide-this-post"
        when (isJust hb) $ do
          void $ runDB $ update postKey [PostHellbanned =. True]
          deleteSession "hide-this-post"
        -- delete old threads
        let tl = boardThreadLimit boardVal
          in when (tl >= 0) $
               flip deletePosts False =<< runDB (selectList [PostBoard ==. board, PostParent ==. 0] [Desc PostBumped, OffsetBy tl])
        -------------------------------------------------------------------------------------------------------
        case name of
          Just name' -> setSession "name" name'
          Nothing    -> deleteSession "name"
        deleteSession "filtered-message"
        deleteSession "message"
        deleteSession "post-title"
        cleanBoardStats board
        unless (hellbannedUID || hellbannedIP) $ sendNewPostES board
        incPostCount
        case goback of
          ToBoard  -> setSession "goback" "ToBoard"  >> redirect (BoardNoPageR board )
          ToThread -> setSession "goback" "ToThread" >> redirect (ThreadR      board nextId)
          ToFeed   -> setSession "goback" "ToFeed"   >> trickyRedirect "ok" (Left MsgPostSent) FeedR
