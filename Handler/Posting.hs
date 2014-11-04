{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Posting where

import           Import
import           Yesod.Routes.Class      (Route)
import           Data.Digest.OpenSSL.MD5 (md5sum)
import           Data.Conduit            (($$))
import qualified Data.Text               as T
import qualified Data.ByteString         as BS
import qualified Data.Conduit.List       as CL
import           Utils.Image
-------------------------------------------------------------------------------------------------------------------
-- This file contains some common forms and helpers for Thread.hs, Board.hs and Edit.hs
-------------------------------------------------------------------------------------------------------------------
data GoBackTo = ToThread | ToBoard
    deriving (Show, Read, Eq, Enum, Bounded)

-------------------------------------------------------------------------------------------------------------------
-- Forms
-------------------------------------------------------------------------------------------------------------------
postForm :: Int        -> -- ^ The maximium length of post title
           Int        -> -- ^ The maximium length of poster name
           Board      -> -- ^ Board value
           Maybe Text -> -- ^ Captcha info
           Html       -> -- ^ Extra token
           MForm Handler (FormResult ( Maybe Text     -- ^ Poster name
                                     , Maybe Text     -- ^ Thread subject
                                     , Maybe Textarea -- ^ Message
                                     , Text           -- ^ Password
                                     , Maybe Text     -- ^ Captcha value
                                     , [FormResult (Maybe FileInfo)] -- ^ Files
                                     , [FormResult Censorship]       -- ^ Censorship ratings
                                     , GoBackTo       -- ^ Go back to
                                     , Maybe Bool     -- ^ No bump
                                     )
                         , Board        -> -- ^ boardW
                           Bool         -> -- ^ isthreadW
                           Maybe Text   -> -- ^ acaptchaW
                           Bool         -> -- ^ enableCaptchaW
                           Maybe (Entity User) -> -- ^ muserW
                           Widget)
postForm maxLenOfPostTitle maxLenOfPostName boardVal maybeCaptchaInfo extra = do
  lastName    <- lookupSession "name"
  lastGoback  <- lookupSession "goback"
  lastMessage <- lookupSession "message"
  lastTitle   <- lookupSession "post-title"
  deleteSession "message"
  deleteSession "post-title"
  msgrender   <- getMessageRender

  let maxMessageLength = boardMaxMsgLength  boardVal
      defaultName      = boardDefaultName   boardVal
      allowedTypes     = boardAllowedTypes  boardVal
      thumbSize        = boardThumbSize     boardVal
      numberFiles      = boardNumberFiles   boardVal
      bumpLimit        = boardBumpLimit     boardVal
      enableCaptcha    = boardEnableCaptcha boardVal
      replyFile        = boardReplyFile     boardVal
      forcedAnon       = boardEnableForcedAnon boardVal
      myMessageField   = checkBool (not . tooLongMessage maxMessageLength)
                                   (MsgTooLongMessage maxMessageLength )
                                   textareaField
  let captchaHelper "Bold"    = msgrender MsgBoldChars
      captchaHelper "Italic"  = msgrender MsgItalicChars
      captchaHelper "Regular" = msgrender MsgRegularChars
      captchaHelper _         = msgrender MsgUnknownError
      urls :: [(Text, GoBackTo)]
      urls = [(msgrender MsgToThread, ToThread), (msgrender MsgToBoard, ToBoard)]
      ratings :: [(Text, Censorship)]
      ratings = map (showText &&& id) [minBound..maxBound]
      fInput       lbl = lbl { fsAttrs = [("onchange","handleFiles(this)"),("class","file-input")] }
      captchaInput lbl = lbl { fsAttrs = ("autocomplete","off") :
                                         maybe [] (\x -> [("placeholder",msgrender MsgTypeOnly <> " " <> captchaHelper x)]) maybeCaptchaInfo
                             }
      acInput      lbl = lbl { fsAttrs = [("autocomplete","off")] }
      nameInput    lbl = lbl { fsAttrs = [("autocomplete","off"),("maxlength",showText maxLenOfPostName )] }
      subjectInput lbl = lbl { fsAttrs = [("autocomplete","off"),("maxlength",showText maxLenOfPostTitle)] }
  ----------------------------------------------------------------------------------------------------------------
  (nameRes     , nameView    ) <- mopt textField              (nameInput    "") (Just <$> lastName)
  (subjectRes  , subjectView ) <- mopt textField              (subjectInput "") (Just <$> lastTitle)
  (messageRes  , messageView ) <- mopt myMessageField                       ""  ((Just . Textarea) <$> lastMessage)
  (passwordRes , passwordView) <- mreq passwordField          (acInput      "") Nothing
  (captchaRes  , captchaView ) <- mopt textField              (captchaInput "") Nothing
  (gobackRes   , gobackView  ) <- mreq (selectFieldList urls)               ""  (Just $ maybe ToBoard (\x -> read $ unpack x :: GoBackTo) lastGoback)
  (nobumpRes   , nobumpView  ) <- mopt checkBoxField                        ""  Nothing
  (fileresults , fileviews   ) <- unzip <$> forM ([1..numberFiles] :: [Int]) (\_ -> mopt fileField (fInput "") Nothing)
  (ratingresults, ratingviews) <- unzip <$> forM ([1..numberFiles] :: [Int]) (\_ -> mreq (selectFieldList ratings) "" Nothing)
  let result = (,,,,,,,,) <$>  nameRes <*> subjectRes  <*> messageRes <*> passwordRes <*> captchaRes <*>
               FormSuccess fileresults <*> FormSuccess ratingresults  <*> gobackRes   <*> nobumpRes
      widget boardW isthreadW acaptchaW enableCaptchaW muserW = $(widgetFile "post-form")
  return (result, widget)
-------------------------------------------------------------------------------------------------------------------
editForm :: [Permission] -> Html -> MForm Handler (FormResult (Textarea, Text, Int, Maybe Bool), Widget)
editForm permissions extra = do
  (postIdRes  , postIdView  ) <- mreq intField      "" Nothing
  (messageRes , messageView ) <- mreq textareaField "" Nothing
  (passwordRes, passwordView) <- mreq passwordField "" Nothing
  (shadowRes  , shadowView  ) <- mopt checkBoxField "" Nothing
  msgrender <- getMessageRender
  let result = (,,,) <$> messageRes <*> passwordRes <*> postIdRes <*> shadowRes
      widget = $(widgetFile "edit-form")
  return (result, widget)
-------------------------------------------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------------------------------------------
isFileAllowed :: [String] -> FormResult (Maybe FileInfo) -> Bool
isFileAllowed allowedTypes (FormSuccess (Just x)) = typeOfFile x `elem` allowedTypes
isFileAllowed _            _                      = True

noMessage :: Maybe Textarea -> Bool
noMessage message = maybe True (T.null . T.filter (`notElem`" \r\n\t") . unTextarea) message

noFiles :: forall a. [FormResult (Maybe a)] -> Bool
noFiles files = all (\(FormSuccess f) -> isNothing f) files

tooLongMessage :: Int -> Textarea -> Bool
tooLongMessage maxLen message = maxLen <= T.length (unTextarea message)
-------------------------------------------------------------------------------------------------------------------
insertFiles :: [FormResult (Maybe FileInfo)] -> -- ^ Files
              [FormResult Censorship]       -> -- ^ Censorship ratings 
               Int      -> -- ^ Thumbnail height and width
               Key Post -> -- ^ Post key
               Handler [Entity Attachedfile]
insertFiles []    _       _         _      = return []
insertFiles files ratings thumbSize postId = forM (filter myFilter $ zip files ratings) insertFile
  where myFilter (FormSuccess (Just _), _) = True
        myFilter _                         = False
        insertFile (FormSuccess (Just f), rating) = do
          let filetype = typeOfFile f
          md5                              <- md5sum <$> BS.concat <$> (fileSource f $$ CL.consume)
          (origfilename, uploadedfilename) <- liftIO $ writeToServer  f md5
          filepath                         <- return $ imageFilePath  filetype uploadedfilename
          filesize                         <- liftIO $ formatFileSize <$> getFileSize filepath
          newFile                          <- return  Attachedfile { attachedfileParentId    = postId
                                                                  , attachedfileMd5         = pack md5
                                                                  , attachedfileName        = uploadedfilename
                                                                  , attachedfileOrigName    = origfilename
                                                                  , attachedfileType        = filetype
                                                                  , attachedfileThumbSize   = thumbSize
                                                                  , attachedfileSize        = pack filesize
                                                                  , attachedfileThumbWidth  = 0
                                                                  , attachedfileThumbHeight = 0
                                                                  , attachedfileWidth       = 0
                                                                  , attachedfileHeight      = 0
                                                                  , attachedfileRating      = (\(FormSuccess r) -> showText r) rating
                                                                  }
          if isImageFile filetype
            then do
              (imgW  , imgH  ) <- liftIO $ getImageResolution filepath
              (thumbW, thumbH) <- liftIO $ makeThumbImg thumbSize filepath uploadedfilename filetype (imgW, imgH)
              let newFile' = newFile { attachedfileWidth       = imgW
                                     , attachedfileHeight      = imgH
                                     , attachedfileThumbWidth  = thumbW
                                     , attachedfileThumbHeight = thumbH
                                     }
              newFileKey <- runDB $ insert newFile'
              return $ Entity newFileKey newFile'
            else do
              liftIO $ makeThumbNonImg uploadedfilename filetype
              newFileKey <- runDB $ insert newFile
              return $ Entity newFileKey newFile
        insertFile _ = error "This never happens"
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
-- | Check a if ban has expired
isBanExpired :: Entity Ban -> Handler Bool
isBanExpired (Entity banId ban) =
  case banExpires ban of
    Nothing   -> return False
    Just t    -> do
      now <- liftIO getCurrentTime
      if now > t
        then runDB (delete banId) >> return True
        else return False
-------------------------------------------------------------------------------------------------------------------      
-- | If an ajax request, redirects to page that makes JSON from message and status string.
--   If a regular request, redirects to given URL.
-- trickyRedirect :: forall (m :: * -> *) b msg url.
--                   (RedirectUrl
--                    (HandlerSite m)
--                    (Route App),
--                    RedirectUrl (HandlerSite m) url, MonadHandler m,
--                    RenderMessage (HandlerSite m) msg) =>
--                   Text -> msg -> url -> m b
trickyRedirect status msg url = do
  setMessageI msg
  t <- isAjaxRequest
  if t
    then redirect (JsonFromMsgR status)
    else redirectUltDest url
