{-# LANGUAGE ScopedTypeVariables #-}
module Handler.API where

import           Import
import qualified Data.Text as T
import           Data.Digest.OpenSSL.MD5 (md5sum)
import qualified Data.ByteString.UTF8   as B

import Handler.Thread  (getThreadR)
import Handler.Board   (getBoardR)
import Handler.Feed    (getAjaxFeedOffsetR)
import Handler.Catalog (getCatalogR)
import Handler.Home    (getHomeR)
import Handler.Ajax    (getAjaxPostByIdR)
import Handler.Delete  (getDeletedByOpR)
import Handler.Posting (checkBan, checkWordfilter, checkTooFastPosting, bumpThread, tooLongMessage)
import Handler.Captcha (checkCaptcha, updateCaptcha)
import Handler.Common  (deletePosts) -- TODO: merge Posting and Common

import Utils.YobaMarkup (doYobaMarkup)
import Utils.File       (createTempFile, insertTempFile)

getApiHomeR :: Handler TypedContent
getApiHomeR = getHomeR

getApiThreadR :: Text -> Int -> Handler TypedContent
getApiThreadR      = getThreadR

getApiBoardR :: Text -> Int -> Handler TypedContent
getApiBoardR       = getBoardR

getApiFeedR :: Int -> Handler TypedContent
getApiFeedR        = getAjaxFeedOffsetR

getApiCatalogR :: Text -> Handler TypedContent
getApiCatalogR     = getCatalogR

getApiDeletedPosts :: Text -> Int -> Handler TypedContent
getApiDeletedPosts = getDeletedByOpR

getApiCaptchaR :: Handler TypedContent
getApiCaptchaR = do
 (path, hint) <- updateCaptcha
 selectRep $ do
   provideJson $ object ["path" .= path, "hint" .= hint]

getApiListBoardsR :: Handler TypedContent
getApiListBoardsR = do
  group  <- (fmap $ userGroup . entityVal) <$> maybeAuth
  boards <- runDB $ selectList ([]::[Filter Board]) []
  config <- getConfigEntity
  let boardCategories = configBoardCategories config ++ [""]
      categoryLabel label = case label of
        "" -> "NO_CATEGORY"
        _  -> label
      filteredBoards  = object $ map (\c -> (categoryLabel c) .= filterBoards boards c group) boardCategories
  selectRep $ do
    provideJson $ filteredBoards
    
getApiPostByIdR :: Int -> Handler TypedContent
getApiPostByIdR = getAjaxPostByIdR

putApiPostR :: Handler TypedContent
putApiPostR = do
  PostRequest{..} <- (requireJsonBody :: Handler PostRequest)
  let isNewThread = parent == 0
      isReply     = not isNewThread
  
  msgrender <- getMessageRender
  --- Validation
  ----------------------------------------------------------------------------------------------------
  mBoard <- (fmap entityVal) <$> runDB (getBy $ BoardUniqName board)
  when (isNothing mBoard) $ sendResponseStatus status404 (object ["message" .= ("BOARD DOES NOT EXIST"::Text)])
  boardVal@Board{..} <- return $ fromJust mBoard
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  hasAccess <- checkViewAccess' mgroup boardVal
  unless hasAccess $ sendResponseStatus status403 (object ["message" .= ("FORBIDDEN" :: Text)])
  mParent <- if isNewThread then return Nothing else runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. parent] []
  when (isNothing mParent && isReply) $ sendResponseStatus status404 (object ["message" .= ("PARENT THREAD DOES NOT EXIST"::Text)])
  let parentVal = entityVal $ fromJust mParent
  when (isReply && maybe True (postLocked . entityVal) mParent) $ sendResponseStatus status403 (object ["message" .= ("THREAD IS LOCKED"::Text)])
  when (isReply && boardReplyFile == "Disabled" && ((/=0) $ length files)) $ sendResponseStatus status412 (object ["message" .= ("FILES DISABLED"::Text)])
  when (isReply && boardReplyFile == "Required" && ((==0) $ length files)) $ sendResponseStatus status412 (object ["message" .= ("A FILE REQUIRED"::Text)])
  when (isNewThread && boardOpFile == "Disabled" && ((/=0) $ length files)) $ sendResponseStatus status412 (object ["message" .= ("FILES DISABLED"::Text)])
  when (isNewThread && boardOpFile == "Required" && ((==0) $ length files)) $ sendResponseStatus status412 (object ["message" .= ("A FILE REQUIRED"::Text)])
  when ( (T.null $ T.filter (`notElem`(" \r\n\t"::String)) message) && ((==0) $ length files) ) $ sendResponseStatus status412 (object ["message" .= ("A FILE OR MESSAGE REQUIRED"::Text)])
  -- TODO: check if file extension allowed
  ----------------------------------------------------------------------------------------------------
  ip <- pack <$> getIp
  checkBan (tread ip) board $ \(Left m) -> sendResponse (object ["message" .= msgrender m])
  ----------------------------------------------------------------------------------------------------
  posterId <- getPosterId
  let permissions = getPermissions mgroup
  when isReply $ unless (checkHellbanned parentVal permissions posterId) $ sendResponseStatus status404 (object ["message" .= ("PARENT THREAD DOES NOT EXIST"::Text)])
  ----------------------------------------------------------------------------------------------------
  postCount <- lookupSession "post-count"
  Config{..} <- getConfigEntity
  when (maybe True (\x -> tread x < configAdaptiveCaptcha) postCount && boardEnableCaptcha && isNothing muser) $ do
    checkCaptcha captcha (sendResponse $ object ["message" .= msgrender MsgWrongCaptcha])
  ----------------------------------------------------------------------------------------------------
  now <- liftIO getCurrentTime
  when isNewThread $ checkTooFastPosting (PostParent ==. 0) ip now $ (sendResponse $ object ["message" .= msgrender MsgPostingTooFast])
  when isReply  $ checkTooFastPosting (PostParent !=. 0) ip now $ (sendResponse $ object ["message" .= msgrender MsgPostingTooFast])
  ----------------------------------------------------------------------------------------------------
  checkWordfilter (Just $ Textarea title  ) board $ \(Right m) -> (sendResponse $ object ["message" .= m])
  checkWordfilter (Just $ Textarea message) board $ \(Right m) -> (sendResponse $ object ["message" .= m])
  hellbannedByWF <- lookupSession "hide-this-post"
  deleteSession "hide-this-post"
  ----------------------------------------------------------------------------------------------------
  when (configGlobalHellban && maybe True ((==0).tread) postCount) $
    void $ runDB $ insert $ Hellban { hellbanUid = posterId, hellbanIp = "" }
  hellbannedUID <- (>0) <$> runDB (count [HellbanUid ==. posterId])
  hellbannedIP  <- (>0) <$> runDB (count [HellbanIp ==. ip])
  ------------------------------------------------------------------------------------------------------
  mDestUID <- if isJust destPost then fmap (fmap postPosterId) $ runDB $ get ((toSqlKey $ fromIntegral $ fromJust destPost) :: Key Post) else return Nothing
  when (isJust destPost && isReply && isNothing mDestUID) $ sendResponseStatus status404 (object ["message" .= ("DESTINATION POST DOES NOT EXIST"::Text)])
  --- Validation end
  ------------------------------------------------------------------------------------------------------
  AppSettings{..}   <- appSettings <$> getYesod
  country           <- getCountry ip
  filteredMsg       <- lookupSession "filtered-message"
  messageFormatted  <- doYobaMarkup (maybe (Just $ Textarea message) (Just . Textarea) filteredMsg) board parent
  nextId            <- maybe 1 ((+1) . postLocalId . entityVal) <$> runDB (selectFirst [PostBoard ==. board] [Desc PostLocalId])
  let newPost = Post { postBoard        = board
                     , postLocalId      = nextId
                     , postParent       = if isReply then parent else 0
                     , postParentTitle  = if isReply then postTitle parentVal else ""
                     , postMessage      = messageFormatted
                     , postRawMessage   = fromMaybe message filteredMsg
                     , postTitle        = T.take appMaxLenOfPostTitle title
                     , postName         = if boardEnableForcedAnon || T.length name == 0 then boardDefaultName else T.take appMaxLenOfPostName name
                     , postDate         = now
                     , postPassword     = pack $ md5sum $ B.fromString $ unpack password
                     , postBumped       = if isReply then Nothing else Just now
                     , postIp           = ip
                     , postCountry      = (\(code,name') -> GeoCountry code name') <$> country
                     , postLocked       = False
                     , postSticked      = False
                     , postAutosage     = False
                     , postDeleted      = False
                     , postDeletedByOp  = False
                     , postOwner        = userGroup . entityVal <$> muser
                     , postOwnerUser    = userName . entityVal <$> muser
                     , postHellbanned   = hellbannedUID || hellbannedIP || isJust hellbannedByWF
                     , postPosterId     = posterId
                     , postLastModified = Nothing                                                
                     , postLockEditing  = False
                     , postDestUID      = if boardEnablePM && isReply then mDestUID else Nothing
                     }
  postKey <- runDB (insert newPost)
  insertedFiles <- forM files $ \fileId -> do
    mFile <- runDB $ get $ toSqlKey $ fromIntegral fileId
    case mFile of
      Just file -> insertTempFile file boardThumbSize postKey boardOnion
      Nothing   -> return Nothing
  when isReply $ do
    isBumpLimit <- (\x -> x >= boardBumpLimit && boardBumpLimit > 0) <$> runDB (count [PostParent ==. parent])
    unless (nobump || isBumpLimit || postAutosage parentVal) $ bumpThread board parent now
  -- delete old threads
  when (isNewThread && boardThreadLimit >= 0) $
    flip deletePosts False =<< runDB (selectList [PostBoard ==. board, PostParent ==. 0] [Desc PostBumped, OffsetBy boardThreadLimit])
  incPostCount
  sendResponseStatus status201 (object [ "message" .= ("CREATED"::Text)
                                       , "post"    .= Entity postKey newPost
                                       , "files"   .= catMaybes insertedFiles
                                       ])

patchApiPostByIdR :: Int -> Handler TypedContent
patchApiPostByIdR editId = do
  muser       <- maybeAuth
  permissions <- getPermissions <$> getMaybeGroup muser
  EditPostRequest{..} <- (requireJsonBody :: Handler EditPostRequest)
  let postKey = (toSqlKey . fromIntegral) editId :: Key Post
  mPost <- runDB $ get postKey
  let shadowEdit = False
  case mPost of
    Just post -> do
      posterId <- getPosterId
      mBoard <- runDB (getBy $ BoardUniqName $ postBoard post)
      case mBoard of
        Just (Entity _ boardVal) -> do
          msgrender<- getMessageRender
          maxTimes <- getConfig configMaxEditings
          unless (EditPostsP `elem` permissions) $ do
            when (postParent post == 0 && not (boardOpEditing   boardVal)) $
              sendResponse $ object ["message" .= msgrender MsgThreadEditingIsDisabled]
            when (postParent post /= 0 && not (boardPostEditing boardVal)) $
              sendResponse $ object ["message" .= ("MsgPostEditingIsDisabled"::Text)]
            when (postLockEditing post) $ 
              sendResponse $ object ["message" .= msgrender MsgDisabledEditing]
            when (postPosterId post /= posterId &&
                  postPassword post /= (pack $ md5sum $ B.fromString $ unpack editPassword)
                 ) $ sendResponse $ object ["message" .= msgrender MsgPostNotYours]
          let maxMessageLength = boardMaxMsgLength boardVal
            in when (tooLongMessage maxMessageLength (Textarea editMessage)) $
                 sendResponse $ object ["message" .= msgrender (MsgTooLongMessage maxMessageLength)]
          checkWordfilter (Just $ Textarea editMessage) (postBoard post) $ \(Right m) -> sendResponse $ object ["message" .= m]
          filteredMsg <- lookupSession "filtered-message"
          messageFormatted <- doYobaMarkup (maybe (Just $ Textarea editMessage) (Just . Textarea) filteredMsg) (postBoard post) (postParent post)
          history <- runDB $ getBy $ HistoryUniqPostId postKey
          unless (EditPostsP `elem` permissions) $ 
            let z = maybe 0 (length . historyMessages . entityVal) history
              in when (z >= maxTimes) $ sendResponseStatus status200 $ object ["message" .= msgrender (MsgYouAlreadyEditedPost maxTimes)]
          now     <- liftIO getCurrentTime
          let oldMessage = postMessage post
              oldDate    = fromMaybe (postDate post) (postLastModified post)
              oldMessages= maybe [] (historyMessages . entityVal) history
              oldDates   = maybe [] (historyDates    . entityVal) history
              newHistory = History { historyPostId   = postKey
                                   , historyMessages = oldMessage : oldMessages
                                   , historyDates    = oldDate    : oldDates
                                   }
          runDB $ update postKey ([PostMessage =. messageFormatted
                                 , PostRawMessage =. fromMaybe editMessage filteredMsg]
                                 ++[PostLastModified =. Just now | not (ShadowEditP `elem` permissions && shadowEdit)])
          unless (ShadowEditP `elem` permissions && shadowEdit) $
            if isJust history
              then runDB $ replace (entityKey $ fromJust history) newHistory
              else void $ runDB $ insert newHistory
          sendResponse $ object ["message" .= msgrender MsgPostEdited]
        Nothing -> sendResponseStatus status404 $ object ["message" .= ("BOARD NOT FOUND"::Text)]
    Nothing -> sendResponseStatus status404 $ object ["message" .= ("POST NOT FOUND"::Text)]

putApiFileR :: Handler TypedContent
putApiFileR = do
  mFile   <- lookupFile "file"
  mRating <- lookupPostParam "rating"
  case mFile of
    Just file -> do
      case mRating of
        Just rating -> do
          key <- createTempFile file (tread rating)
          sendResponse $ object ["id" .= ((fromIntegral $ fromSqlKey key)::Int)]
        Nothing ->  sendResponse $ object ["message" .= ("INVALID RATING"::Text)]
    Nothing  -> sendResponse $ object ["message" .= ("INVALID FILE"::Text)]

  -- file@FileRequest{..} <- requireJsonBody
  -- let fileData = BS.decode (T.encodeUtf8 fcontent)
  -- case fileData of
  --   Left err -> sendResponseStatus status400 $ object ["message" .= ("CANT DECODE BASE64"::Text)]
  --   Right f  -> do
  --     liftIO $ BS.writeFile "/home/user/testfile" f
  --     liftIO $ print f
