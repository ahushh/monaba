{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Thread where
 
import           Import
import           Yesod.Auth
import           Prelude            (head)
import qualified Data.Text          as T
import qualified Database.Esqueleto as E
import qualified Data.Map.Strict    as Map
import           Control.Arrow      (second)
import           AwfulMarkup        (doAwfulMarkup)
import           Handler.Captcha    (checkCaptcha, recordCaptcha, getCaptchaInfo, updateAdaptiveCaptcha)
import           Handler.Posting

import           Text.Blaze.Html.Renderer.String
-------------------------------------------------------------------------------------------------------------------
-- Костыли-костылики...
getJsonFromMsgR :: Text -> Handler TypedContent
getJsonFromMsgR status = do
  msg <- getMessage
  selectRep $
    provideJson $ object [(status, toJSON $ renderHtml $ fromJust msg)]
-------------------------------------------------------------------------------------------------------------------
getThreadR :: Text -> Int -> Handler Html
getThreadR  board thread = do
  when (thread == 0) notFound
  muser       <- maybeAuth
  maybeBoard  <- runDB $ getBy $ BoardUniqName board
  when (isNothing maybeBoard ) notFound
  boards      <- runDB $ selectList ([]::[Filter Board]) []
  -------------------------------------------------------------------------------------------------------  
  let numberFiles   = boardNumberFiles   $ entityVal $ fromJust maybeBoard
      enableCaptcha = boardEnableCaptcha $ entityVal $ fromJust maybeBoard
  -------------------------------------------------------------------------------------------------------
  allPosts' <- runDB $ E.select $ E.from $ \(post `E.LeftOuterJoin` file) -> do
    E.on $ (E.just (post E.^. PostId)) E.==. (file E.?. AttachedfileParentId)
    E.where_ ((post E.^. PostBoard ) E.==. (E.val board ) E.&&.
             ((post E.^. PostParent) E.==. (E.val thread) E.||.
             ((post E.^. PostParent) E.==. (E.val 0     ) E.&&. (post E.^. PostLocalId) E.==. (E.val thread))))
    E.orderBy [E.asc (post E.^. PostId)]
    return (post, file)
  when (null allPosts') notFound
  let allPosts        = map (second catMaybes) $ Map.toList $ keyValuesToMap allPosts'
      repliesAndFiles = drop 1 allPosts
      eOpPost         = fst $ head allPosts
      opPostFiles     = snd $ head allPosts 
      pagetitle       = (\t -> if T.null t then pack $ show $ postLocalId (entityVal eOpPost) else t) $ postTitle (entityVal eOpPost)
  -------------------------------------------------------------------------------------------------------
  acaptcha  <- lookupSession "acaptcha"
  when (isNothing acaptcha && enableCaptcha && isNothing muser) $ recordCaptcha =<< getConfig configCaptchaLength
  ------------------------------------------------------------------------------------------------------- 
  (formWidget, formEnctype) <- generateFormPost $ postForm numberFiles
  nameOfTheBoard   <- extraSiteName <$> getExtra
  maybeCaptchaInfo <- getCaptchaInfo
  boardCategories  <- getConfig configBoardCategories
  msgrender        <- getMessageRender
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " - ", board, " - ", pagetitle]
    $(widgetFile "thread")
-------------------------------------------------------------------------------------------------------------------
postThreadR :: Text -> Int -> Handler Html
postThreadR board thread = do
  maybeParent <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  maybeBoard  <- runDB $ getBy $ BoardUniqName board
  muser       <- maybeAuth
  -------------------------------------------------------------------------------------------------------     
  when (isNothing maybeParent) notFound
  when (isNothing maybeBoard ) notFound
  -------------------------------------------------------------------------------------------------------     
  let maxMessageLength     = boardMaxMsgLength  $ entityVal $ fromJust maybeBoard
      defaultName          = boardDefaultName   $ entityVal $ fromJust maybeBoard
      allowedTypesToUpload = boardAllowedTypes  $ entityVal $ fromJust maybeBoard
      thumbSize            = boardThumbSize     $ entityVal $ fromJust maybeBoard
      numberFiles          = boardNumberFiles   $ entityVal $ fromJust maybeBoard
      bumpLimit            = boardBumpLimit     $ entityVal $ fromJust maybeBoard
      enableCaptcha        = boardEnableCaptcha $ entityVal $ fromJust maybeBoard
      threadUrl            = ThreadR board thread
      isFileAllowed (FormSuccess (Just x)) = typeOfFile x `elem` allowedTypesToUpload
      isFileAllowed _                      = True
  -------------------------------------------------------------------------------------------------------         
  ((result, _), _) <- runFormPost $ postForm numberFiles
  case result of
    FormFailure _                            -> trickyRedirect "error" MsgBadFormData threadUrl
    FormMissing                              -> trickyRedirect "error" MsgNoFormData  threadUrl
    FormSuccess (name, title, message, pswd, captcha, files, goback, Just nobump)
      | (\(Just (Entity _ p)) -> postLocked p) maybeParent                          -> trickyRedirect "error" MsgLockedThread threadUrl
      | isNothing message && all (\(FormSuccess f) -> isNothing f) files             -> trickyRedirect "error" MsgNoImgOrText  threadUrl
      | maxMessageLength <= T.length (unTextarea $ fromMaybe (Textarea "") message) -> trickyRedirect "error" (MsgTooLongMessage maxMessageLength) threadUrl
      | maybe False (T.null . T.filter (`notElem`" \r\n\t") . unTextarea) message  -> trickyRedirect "error" MsgNoImgOrText    threadUrl
      | not $ all isFileAllowed files                                                -> trickyRedirect "error" MsgTypeNotAllowed threadUrl
      | otherwise                                                                  -> do
        setSession "message"    (maybe     "" unTextarea message)
        setSession "post-title" (fromMaybe "" title)
        ip  <- pack <$> getIp
        -- check ban
        ban <- runDB $ selectFirst [BanIp ==. ip] [Desc BanId]
        when (isJust ban) $
          unlessM (isBanExpired $ fromJust ban) $ do
            let m =  MsgYouAreBanned (banReason $ entityVal $ fromJust ban)
                                     (maybe "never" (pack . myFormatTime) (banExpires $ entityVal $ fromJust ban))
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
        messageFormatted <- doAwfulMarkup message board thread
        lastPost'        <- runDB (selectFirst [PostBoard ==. board] [Desc PostLocalId])
        when (isNothing lastPost') $  -- reply to non-existent thread
          trickyRedirect "error" MsgNoSuchThread (BoardNoPageR board)
        let nextId  = 1 + postLocalId (entityVal $ fromJust lastPost')
            newPost = Post { postBoard        = board
                           , postLocalId      = nextId
                           , postParent       = thread
                           , postMessage      = messageFormatted
                           , postTitle        = maybe ("" :: Text) (T.take 30) title
                           , postName         = maybe defaultName (T.take 10) name
                           , postDate         = now
                           , postPassword     = pswd
                           , postBumped       = Nothing
                           , postIp           = ip
                           , postLocked       = False
                           , postSticked      = False
                           , postAutosage     = False
                           -- , postDeleted      = False
                           -- , postDeletedByOp  = False
                           , postOwner        = (personRole . entityVal) <$> muser
                           , postPosterId     = posterId
                           }
        void $ insertFiles files thumbSize =<< runDB (insert newPost)
        ------------------------------------------------------------------------------------------------------- 
        isBumpLimit <- (\x -> x >= bumpLimit && bumpLimit > 0) <$> runDB (count [PostParent ==. thread])
        unless (nobump || isBumpLimit || postAutosage (entityVal $ fromJust maybeParent)) $ bumpThread board thread now
        when (isJust name) $ setSession "name" (fromMaybe defaultName name)
        deleteSession "message"
        deleteSession "post-title"
        case goback of
          ToBoard  -> setSession "goback" "ToBoard"  >> trickyRedirect "ok" MsgPostSent (BoardNoPageR board)
          ToThread -> setSession "goback" "ToThread" >> trickyRedirect "ok" MsgPostSent threadUrl
    _  -> trickyRedirect "error" MsgUnknownError threadUrl
