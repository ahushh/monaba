{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Board where

import           Import
import           Yesod.Auth
import qualified Data.Text       as T
import           Handler.Delete  (deletePosts)
import           Handler.Captcha (checkCaptcha, recordCaptcha, getCaptchaInfo, updateAdaptiveCaptcha)
import           Handler.Posting
import           Utils.YobaMarkup (doYobaMarkup)
--------------------------------------------------------------------------------------------------------- 
getBoardNoPageR :: Text -> Handler Html
getBoardNoPageR board = getBoardR board 0

postBoardNoPageR :: Text -> Handler Html
postBoardNoPageR board = postBoardR board 0
--------------------------------------------------------------------------------------------------------- 
selectThreadsAndPreviews :: Text ->
                           Int  ->
                           Int  ->
                           Int  ->
                           Text ->
                           [Permission] ->
                           Handler [(  (Entity Post, [Entity Attachedfile])
                                    , [(Entity Post, [Entity Attachedfile])]
                                    , Int
                                    )]
selectThreadsAndPreviews board page threadsPerPage previewsPerThread posterId permissions =
  let selectThreadsAll = selectList [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False]
                         [Desc PostSticked, Desc PostBumped, LimitTo threadsPerPage, OffsetBy $ page*threadsPerPage]
      selectThreadsHB  = selectList ( [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False, PostHellbanned ==. False] ||.
                                      [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False
                                      ,PostHellbanned ==. True, PostPosterId ==. posterId]
                                    )
                         [Desc PostSticked, Desc PostBumped, LimitTo threadsPerPage, OffsetBy $ page*threadsPerPage]
      selectThreads    = if HellBanP `elem` permissions then selectThreadsAll else selectThreadsHB
      --------------------------------------------------------------------------------------------------
      selectFiles  pId = selectList [AttachedfileParentId ==. pId] []
      --------------------------------------------------------------------------------------------------
      selectPreviews   = if HellBanP `elem` permissions then selectPreviewsAll else selectPreviewsHB
      selectPreviewsHB t
        | previewsPerThread > 0 = selectList ( [PostDeletedByOp ==. False, PostBoard ==. board, PostDeleted ==. False,
                                                PostParent ==. postLocalId t, PostHellbanned ==. False] ||.
                                               [PostDeletedByOp ==. False, PostBoard ==. board, PostDeleted ==. False,
                                                PostParent ==. postLocalId t, PostHellbanned ==. True,
                                                PostPosterId ==. posterId]
                                             )
                                  [Desc PostDate, LimitTo previewsPerThread]
        | otherwise             = return []
      selectPreviewsAll t
        | previewsPerThread > 0 = selectList [PostDeletedByOp ==. False, PostBoard ==. board, PostDeleted ==. False
                                             ,PostParent ==. postLocalId t] [Desc PostDate, LimitTo previewsPerThread]
        | otherwise             = return []
      --------------------------------------------------------------------------------------------------
      countPostsAll t = [PostDeletedByOp ==. False, PostDeleted ==. False
                        ,PostBoard ==. board, PostParent ==. postLocalId t]
      countPostsHB  t = [PostDeletedByOp ==. False, PostDeleted ==. False ,PostBoard ==. board
                        ,PostParent ==. postLocalId t, PostHellbanned ==. False] ||. 
                        [PostDeletedByOp ==. False, PostDeleted ==. False ,PostBoard ==. board
                        ,PostParent ==. postLocalId t, PostHellbanned ==. True, PostPosterId ==. posterId]
      countPosts t = count (if HellBanP `elem` permissions then countPostsAll t else countPostsHB t)
  in runDB $ selectThreads >>= mapM (\th@(Entity tId t) -> do
       threadFiles      <- selectFiles tId
       previewsAndFiles <- selectPreviews t >>= mapM (\pr -> do
         previewFiles <- selectFiles $ entityKey pr
         return (pr, previewFiles))
       postsInThread <- countPosts t
       return ((th, threadFiles), reverse previewsAndFiles, postsInThread - previewsPerThread))

getBoardR :: Text -> Int -> Handler Html
getBoardR board page = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let hasAccessToNewThread = checkAccessToNewThread mgroup boardVal
      permissions          = getPermissions mgroup
  ------------------------------------------------------------------------------------------------------- 
  numberOfThreads <- runDB $ count [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False, PostHellbanned ==. False]
  posterId        <- getPosterId
  let numberFiles       = boardNumberFiles       boardVal
      maxMessageLength  = boardMaxMsgLength      boardVal
      threadsPerPage    = boardThreadsPerPage    boardVal
      previewsPerThread = boardPreviewsPerThread boardVal
      enableCaptcha     = boardEnableCaptcha     boardVal
      boardDesc         = boardDescription       boardVal
      boardLongDesc     = boardLongDescription   boardVal
      geoIpEnabled      = boardEnableGeoIp       boardVal
      pages             = listPages threadsPerPage numberOfThreads
  threadsAndPreviews <- selectThreadsAndPreviews board page threadsPerPage previewsPerThread posterId permissions
  ------------------------------------------------------------------------------------------------------- 
  geoIps' <- forM (if geoIpEnabled then threadsAndPreviews else []) $ \((Entity tId t,_),ps,_) -> do
    xs <- forM ps $ \(Entity pId p,_) -> getCountry (postIp p) >>= (\c' -> return (pId, c'))
    c  <- getCountry $ postIp t
    return $ (tId, c):xs
  let geoIps = map (second fromJust) $ filter (isJust . snd) $ concat geoIps'
  -------------------------------------------------------------------------------------------------------
  now       <- liftIO getCurrentTime
  acaptcha  <- lookupSession "acaptcha"
  when (isNothing acaptcha && enableCaptcha && isNothing muser) $ recordCaptcha =<< getConfig configCaptchaLength
  ------------------------------------------------------------------------------------------------------- 
  (formWidget, formEnctype) <- generateFormPost $ postForm boardVal
  (formWidget', _)          <- generateFormPost editForm
  nameOfTheBoard   <- extraSiteName <$> getExtra
  maybeCaptchaInfo <- getCaptchaInfo
  msgrender        <- getMessageRender
  timeZone         <- getTimeZone
  rating           <- getCensorshipRating
  defaultLayout $ do
    setUltDestCurrent
    let p = if page > 0 then T.concat [" (", pack (show page), ")"] else ""
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", boardDesc, p]
    $(widgetFile "board")
--------------------------------------------------------------------------------------------------------- 
postBoardR :: Text -> Int -> Handler Html
postBoardR board _ = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  -------------------------------------------------------------------------------------------------------   
  let msgRedirect msg  = setMessageI msg >> redirect (BoardNoPageR board)
      defaultName      = boardDefaultName   boardVal
      allowedTypes     = boardAllowedTypes  boardVal
      thumbSize        = boardThumbSize     boardVal
      enableCaptcha    = boardEnableCaptcha boardVal
      opFile           = boardOpFile        boardVal
  -------------------------------------------------------------------------------------------------------       
  ((result, _),   _) <- runFormPost $ postForm boardVal
  case result of
    FormFailure []                     -> msgRedirect MsgBadFormData
    FormFailure xs                     -> msgRedirect $ MsgError $ T.intercalate "; " xs
    FormMissing                        -> msgRedirect MsgNoFormData
    FormSuccess (name, title, message, pswd, captcha, files, ratings, goback, Just _)
      | opFile == "Disabled"&& not (noFiles files)      -> msgRedirect MsgOpFileIsDisabled
      | opFile == "Required"&& noFiles files          -> msgRedirect MsgNoFile
      | noMessage message  && noFiles files          -> msgRedirect MsgNoFileOrText
      | not $ all (isFileAllowed allowedTypes) files  -> msgRedirect MsgTypeNotAllowed
      | otherwise                                   -> do
        -- save form values in case something goes wrong
        setSession "message"    (maybe     "" unTextarea message)
        setSession "post-title" (fromMaybe "" title)
        -- check ban
        ip  <- pack <$> getIp
        ban <- runDB $ selectFirst [BanIp ==. ip] [Desc BanId]
        when (isJust ban) $ 
          unlessM (isBanExpired $ fromJust ban) $ do
            setMessageI $ MsgYouAreBanned (banReason $ entityVal $ fromJust ban)
                                          (maybe "never" (pack . myFormatTime 0) (banExpires $ entityVal $ fromJust ban))
            redirect (BoardNoPageR board)
        -- check captcha
        acaptcha <- lookupSession "acaptcha"
        when (enableCaptcha && isNothing muser) $ do
          when (isNothing acaptcha) $ do
            void $ when (isNothing captcha) (setMessageI MsgWrongCaptcha >> redirect (BoardNoPageR board))
            checkCaptcha (fromJust captcha) (setMessageI MsgWrongCaptcha >> redirect (BoardNoPageR board))
          updateAdaptiveCaptcha acaptcha
        -------------------------------------------------------------------------------------------------------
        now      <- liftIO getCurrentTime
        -- check too fast posting
        lastPost <- runDB $ selectFirst [PostIp ==. ip, PostParent ==. 0] [Desc PostDate] -- last thread by IP
        when (isJust lastPost) $ do
          let diff = ceiling ((realToFrac $ diffUTCTime now (postDate $ entityVal $ fromJust lastPost)) :: Double)
          whenM ((>diff) <$> getConfig configReplyDelay) $ 
            deleteSession "acaptcha" >>
            setMessageI MsgPostingTooFast >> redirect (BoardNoPageR board)
        ------------------------------------------------------------------------------------------------------
        posterId   <- getPosterId
        hellbanned <- (>0) <$> runDB (count [HellbanUserId ==. posterId])
        nextId <- maybe 1 ((+1) . postLocalId . entityVal) <$> runDB (selectFirst [PostBoard ==. board] [Desc PostLocalId])
        messageFormatted <- doYobaMarkup message board 0
        let newPost = Post { postBoard        = board
                           , postLocalId      = nextId
                           , postParent       = 0
                           , postMessage      = messageFormatted
                           , postRawMessage   = maybe "" unTextarea message
                           , postTitle        = maybe ("" :: Text) (T.take 60) title
                           , postName         = maybe defaultName (T.take 20) name
                           , postDate         = now
                           , postPassword     = pswd
                           , postBumped       = Just now
                           , postIp           = ip
                           , postLocked       = False
                           , postSticked      = False
                           , postAutosage     = False
                           , postDeleted      = False
                           , postDeletedByOp  = False
                           , postOwner        = pack . show . userGroup . entityVal <$> muser
                           , postHellbanned   = hellbanned
                           , postPosterId     = posterId
                           , postLastModified = Nothing
                           }
        void $ insertFiles files ratings thumbSize =<< runDB (insert newPost)
        -- delete old threads
        let tl = boardThreadLimit boardVal
          in when (tl >= 0) $
               flip deletePosts False =<< runDB (selectList [PostBoard ==. board, PostParent ==. 0] [Desc PostBumped, OffsetBy tl])
        -------------------------------------------------------------------------------------------------------
        -- remember poster name
        when (isJust name) $ setSession "name" (fromMaybe defaultName name) 
        -- everything went well, delete these values
        deleteSession "message"
        deleteSession "post-title"
        case goback of
          ToBoard  -> setSession "goback" "ToBoard"  >> redirect (BoardNoPageR board )
          ToThread -> setSession "goback" "ToThread" >> redirect (ThreadR      board nextId)
    _  -> msgRedirect MsgUnknownError
