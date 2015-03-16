{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Board where

import           Import
import           Yesod.Auth
import qualified Data.Text       as T
import           Handler.Delete  (deletePosts)
import           Handler.Posting
import           Handler.Captcha (checkCaptcha)
import           Utils.File            (insertFiles)
import           Utils.YobaMarkup      (doYobaMarkup)
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
  let selectThreads = selectList [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False]
                      [Desc PostSticked, Desc PostBumped, LimitTo threadsPerPage, OffsetBy $ page*threadsPerPage]
      --------------------------------------------------------------------------------------------------
      selectFiles  pId = selectList [AttachedfileParentId ==. pId] []
      --------------------------------------------------------------------------------------------------
      selectPreviews t
        | previewsPerThread > 0 = selectList [PostDeletedByOp ==. False, PostBoard ==. board, PostDeleted ==. False
                                             ,PostParent ==. postLocalId t] [Desc PostDate, LimitTo previewsPerThread]
        | otherwise             = return []
      --------------------------------------------------------------------------------------------------
      countPosts t = count [PostDeletedByOp ==. False, PostDeleted ==. False, PostBoard ==. board, PostParent ==. postLocalId t]
  in runDB $ selectThreads >>= mapM (\th@(Entity tId t) -> do
       threadFiles      <- selectFiles tId
       previewsAndFiles <- selectPreviews t >>= mapM (\pr -> do
         previewFiles <- selectFiles $ entityKey pr
         return (pr, previewFiles))
       postsInThread <- countPosts t
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
  numberOfThreads <- runDB $ count [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False]
  posterId        <- getPosterId
  let numberFiles       = boardNumberFiles       boardVal
      maxMessageLength  = boardMaxMsgLength      boardVal
      threadsPerPage    = boardThreadsPerPage    boardVal
      previewsPerThread = boardPreviewsPerThread boardVal
      title             = boardTitle             boardVal
      summary           = boardSummary           boardVal
      geoIpEnabled      = boardEnableGeoIp       boardVal
      pages             = listPages threadsPerPage numberOfThreads
  threadsAndPreviews <- selectThreadsAndPreviews board page threadsPerPage previewsPerThread posterId permissions
  ------------------------------------------------------------------------------------------------------- 
  maxLenOfPostTitle <- extraMaxLenOfPostTitle <$> getExtra
  maxLenOfPostName  <- extraMaxLenOfPostName  <$> getExtra
  (formWidget, formEnctype) <- generateFormPost $ postForm maxLenOfPostTitle maxLenOfPostName boardVal muser
  (formWidget', _)          <- generateFormPost editForm
  nameOfTheBoard   <- extraSiteName <$> getExtra
  msgrender        <- getMessageRender
  timeZone         <- getTimeZone

  defaultLayout $ do
    setUltDestCurrent
    let p = if page > 0 then T.concat [" (", showText page, ") "] else ""
    setTitle $ toHtml $ T.concat $ reverse [nameOfTheBoard, titleDelimiter, title, p]
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
      enableCaptcha    = boardEnableCaptcha boardVal
  -------------------------------------------------------------------------------------------------------       
  ((result, _),   _) <- runFormPost $ postForm 0 0 boardVal muser
  case result of
    FormFailure []                     -> msgRedirect MsgBadFormData
    FormFailure xs                     -> msgRedirect $ MsgError $ T.intercalate "; " xs
    FormMissing                        -> msgRedirect MsgNoFormData
    FormSuccess (name, title, message, captcha, pswd, files, goback, _)
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
        when (enableCaptcha && isNothing muser) $ do
          checkCaptcha captcha (setMessageI MsgWrongCaptcha >> redirect (BoardNoPageR board))
        -------------------------------------------------------------------------------------------------------
        now      <- liftIO getCurrentTime
        country  <- getCountry ip
        -- check too fast posting
        lastPost <- runDB $ selectFirst [PostIp ==. ip, PostParent ==. 0] [Desc PostDate] -- last thread by IP
        when (isJust lastPost) $ do
          let diff = ceiling ((realToFrac $ diffUTCTime now (postDate $ entityVal $ fromJust lastPost)) :: Double)
          whenM ((>diff) <$> getConfig configReplyDelay) $ 
            setMessageI MsgPostingTooFast >> redirect (BoardNoPageR board)
        ------------------------------------------------------------------------------------------------------
        posterId <- getPosterId
        nextId <- maybe 1 ((+1) . postLocalId . entityVal) <$> runDB (selectFirst [PostBoard ==. board] [Desc PostLocalId])
        messageFormatted  <- doYobaMarkup message board 0
        maxLenOfPostTitle <- extraMaxLenOfPostTitle <$> getExtra
        maxLenOfPostName  <- extraMaxLenOfPostName  <$> getExtra
        let newPost = Post { postBoard        = board
                           , postLocalId      = nextId
                           , postParent       = 0
                           , postMessage      = messageFormatted
                           , postRawMessage   = maybe "" unTextarea message
                           , postTitle        = maybe ("" :: Text) (T.take maxLenOfPostTitle) title
                           , postName         = maybe defaultName (T.take maxLenOfPostName) name
                           , postDate         = now
                           , postPassword     = pswd
                           , postBumped       = Just now
                           , postIp           = ip
                           , postCountry      = (\(code,name') -> GeoCountry code name') <$> country
                           , postLocked       = False
                           , postSticked      = False
                           , postAutosage     = False
                           , postDeleted      = False
                           , postDeletedByOp  = False
                           , postOwner        = showText . userGroup . entityVal <$> muser
                           , postPosterId     = posterId
                           , postLastModified = Nothing
                           }
        void $ insertFiles files thumbSize =<< runDB (insert newPost)
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
