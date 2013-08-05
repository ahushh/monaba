{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Board where

import           Import
import           Yesod.Auth
import qualified Data.Text       as T
import           Handler.Delete  (deletePosts)
import           Handler.Captcha (checkCaptcha, recordCaptcha, getCaptchaInfo, updateAdaptiveCaptcha)
import           Handler.Posting
import           AwfulMarkup     (doAwfulMarkup)
--------------------------------------------------------------------------------------------------------- 
getBoardNoPageR :: Text -> Handler Html
getBoardNoPageR board = getBoardR board 0

postBoardNoPageR :: Text -> Handler Html
postBoardNoPageR board = postBoardR board 0
--------------------------------------------------------------------------------------------------------- 
getBoardR :: Text -> Int -> Handler Html
getBoardR board page = do
  muser   <- maybeAuth
  mgroup  <- case muser of
    Just (Entity _ u) -> runDB $ getBy $ GroupUniqName $ userGroup u
    _                 -> return Nothing
  let permissions = maybe [] (groupPermissions . entityVal) mgroup

  maybeBoard <- runDB $ getBy $ BoardUniqName board
  when (isNothing maybeBoard) notFound

  let group  = (groupName . entityVal) <$> mgroup
      access = boardViewAccess $ entityVal $ fromJust maybeBoard
  when (isJust access && access /= group) notFound
 
  boards     <- runDB $ selectList ([]::[Filter Board]) []
  ------------------------------------------------------------------------------------------------------- 
  numberOfThreads <- runDB $ count [PostBoard ==. board, PostParent ==. 0]
  let numberFiles       = boardNumberFiles       $ entityVal $ fromJust maybeBoard
      threadsPerPage    = boardThreadsPerPage    $ entityVal $ fromJust maybeBoard
      previewsPerThread = boardPreviewsPerThread $ entityVal $ fromJust maybeBoard
      enableCaptcha     = boardEnableCaptcha     $ entityVal $ fromJust maybeBoard
      boardDesc         = boardDescription       $ entityVal $ fromJust maybeBoard
      ---------------------------------------------------------------------------------
      pages             = [0..pagesFix $ floor $ (fromIntegral numberOfThreads :: Double) / (fromIntegral threadsPerPage :: Double)]
      pagesFix x
        | numberOfThreads > 0 && numberOfThreads `mod` threadsPerPage == 0 = x - 1
        | otherwise                                                      = x
      ---------------------------------------------------------------------------------
      selectThreads    = selectList [PostBoard ==. board, PostParent ==. 0]
                         [Desc PostSticked, Desc PostBumped, LimitTo threadsPerPage, OffsetBy $ page*threadsPerPage]
      selectFiles  pId = selectList [AttachedfileParentId ==. pId] []
      selectPreviews t
        | previewsPerThread > 0 = selectList [PostDeletedByOp ==. False, PostBoard ==. board,
                                              PostParent ==. postLocalId t] [Desc PostDate, LimitTo previewsPerThread]
        | otherwise             = return []
  -------------------------------------------------------------------------------------------------------
  threadsAndPreviews <- runDB $ selectThreads >>= mapM (\th@(Entity tId t) -> do
                           threadFiles      <- selectFiles tId
                           previewsAndFiles <- selectPreviews t >>= mapM (\pr -> do
                             previewFiles <- selectFiles $ entityKey pr
                             return (pr, previewFiles))
                           postsInThread <- count [PostDeletedByOp ==. False, PostBoard ==. board, PostParent ==. postLocalId t]
                           return ((th, threadFiles), reverse previewsAndFiles, postsInThread - previewsPerThread))
  ------------------------------------------------------------------------------------------------------- 
  now       <- liftIO getCurrentTime
  acaptcha  <- lookupSession "acaptcha"
  when (isNothing acaptcha && enableCaptcha && isNothing muser) $ recordCaptcha =<< getConfig configCaptchaLength
  ------------------------------------------------------------------------------------------------------- 
  (formWidget, formEnctype) <- generateFormPost $ postForm numberFiles
  nameOfTheBoard   <- extraSiteName <$> getExtra
  maybeCaptchaInfo <- getCaptchaInfo
  boardCategories  <- getConfig configBoardCategories
  msgrender        <- getMessageRender

  -- let userGroup             = maybe Nothing (Just . userGroup . entityVal) muser
      -- hasAccessToNewThread = userGroup >= (boardThreadAccess $ entityVal $ fromJust maybeBoard)
  let hasAccessToNewThread = True
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", board, " — ", boardDesc]
    $(widgetFile "board")
    
postBoardR :: Text -> Int -> Handler Html
postBoardR board _ = do
  maybeBoard <- runDB $ getBy $ BoardUniqName board
  when (isNothing maybeBoard) notFound

  muser   <- maybeAuth
  mgroup  <- case muser of
    Just (Entity _ u) -> runDB $ getBy $ GroupUniqName $ userGroup u
    _                 -> return Nothing
  let permissions = maybe [] (groupPermissions . entityVal) mgroup

  let group  = (groupName . entityVal) <$> mgroup
      access = boardViewAccess $ entityVal $ fromJust maybeBoard
  when (isJust access && access /= group) notFound
  -------------------------------------------------------------------------------------------------------   
  let msgRedirect msg  = setMessageI msg >> redirect (BoardNoPageR board)
      maxMessageLength = boardMaxMsgLength  $ entityVal $ fromJust maybeBoard
      defaultName      = boardDefaultName   $ entityVal $ fromJust maybeBoard
      allowedTypes     = boardAllowedTypes  $ entityVal $ fromJust maybeBoard
      thumbSize        = boardThumbSize     $ entityVal $ fromJust maybeBoard
      numberFiles      = boardNumberFiles   $ entityVal $ fromJust maybeBoard
      enableCaptcha    = boardEnableCaptcha $ entityVal $ fromJust maybeBoard
      opWithoutFile    = boardOpWithoutFile $ entityVal $ fromJust maybeBoard      
  -------------------------------------------------------------------------------------------------------       
  ((result, _),   _) <- runFormPost $ postForm numberFiles
  case result of
    FormFailure _                            -> msgRedirect MsgBadFormData
    FormMissing                              -> msgRedirect MsgNoFormData
    FormSuccess (name, title, message, pswd, captcha, files, goback, Just _)
      | not opWithoutFile   && noFiles files           -> msgRedirect MsgNoFile
      | noMessage message && noFiles files           -> msgRedirect MsgNoFileOrText
      | tooLongMessage message maxMessageLength     -> msgRedirect $ MsgTooLongMessage maxMessageLength
      | not $ all (isFileAllowed allowedTypes) files  -> msgRedirect MsgTypeNotAllowed
      | otherwise                                   -> do
        setSession "message"    (maybe     "" unTextarea message)
        setSession "post-title" (fromMaybe "" title)
        -- check ban
        ip  <- pack <$> getIp
        ban <- runDB $ selectFirst [BanIp ==. ip] [Desc BanId]
        when (isJust ban) $ 
          unlessM (isBanExpired $ fromJust ban) $ do
            setMessageI $ MsgYouAreBanned (banReason $ entityVal $ fromJust ban)
                                          (maybe "never" (pack . myFormatTime) (banExpires $ entityVal $ fromJust ban))
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
        posterId <- getPosterId
        nextId <- maybe 1 ((+1) . postLocalId . entityVal) <$> runDB (selectFirst [PostBoard ==. board] [Desc PostLocalId])
        messageFormatted <- doAwfulMarkup message board 0
        let newPost = Post { postBoard        = board
                           , postLocalId      = nextId
                           , postParent       = 0
                           , postMessage      = messageFormatted
                           , postTitle        = maybe ("" :: Text) (T.take 30) title
                           , postName         = maybe defaultName (T.take 10) name
                           , postDate         = now
                           , postPassword     = pswd
                           , postBumped       = Just now
                           , postIp           = ip
                           , postLocked       = False
                           , postSticked      = False
                           , postAutosage     = False
                           -- , postDeleted      = False
                           , postDeletedByOp  = False
                           , postOwner        = pack . show . userGroup . entityVal <$> muser
                           , postPosterId     = posterId
                           }
        void $ insertFiles files thumbSize =<< runDB (insert newPost)
        -- delete old threads
        let tl = boardThreadLimit $ entityVal $ fromJust maybeBoard
          in when (tl >= 0) $
               deletePosts =<< runDB (selectList [PostBoard ==. board, PostParent ==. 0] [Desc PostBumped, OffsetBy tl])
        -------------------------------------------------------------------------------------------------------
        when (isJust name) $ setSession "name" (fromMaybe defaultName name) 
        deleteSession "message"
        deleteSession "post-title"
        case goback of
          ToBoard  -> setSession "goback" "ToBoard"  >> redirect (BoardNoPageR board )
          ToThread -> setSession "goback" "ToThread" >> redirect (ThreadR      board nextId)
    _  -> msgRedirect MsgUnknownError
