{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Posting where

import           Import
import qualified Data.Text as T
-------------------------------------------------------------------------------------------------------------------
-- This file contains some common forms and helpers for Thread.hs, Board.hs and Edit.hs
-------------------------------------------------------------------------------------------------------------------
data GoBackTo = ToThread | ToBoard | ToFeed
    deriving (Show, Read, Eq, Enum, Bounded)
-------------------------------------------------------------------------------------------------------------------
-- Forms
-------------------------------------------------------------------------------------------------------------------

postForm :: Int   -> -- ^ The maximium length of post title
           Int   -> -- ^ The maximium length of poster name
           Board -> -- ^ Board value
           Maybe (Entity User) -> -- ^ User
           Html                -> -- ^ Extra token
           MForm Handler (FormResult ( Maybe Text     -- ^ Poster name
                                     , Maybe Text     -- ^ Thread subject
                                     , Maybe Textarea -- ^ Message
                                     , Maybe Text     -- ^ Captcha
                                     , Text           -- ^ Password
                                     , [FormResult (Maybe FileInfo)] -- ^ Files
                                     , GoBackTo       -- ^ Go back to
                                     , Maybe Bool     -- ^ No bump
                                     )
                         , Widget)
postForm maxLenOfPostTitle maxLenOfPostName boardVal muser extra = do
  lastName    <- lookupSession "name"
  lastGoback  <- lookupSession "goback"
  lastMessage <- lookupSession "message"
  lastTitle   <- lookupSession "post-title"
  deleteSession "message"
  deleteSession "post-title"
  msgrender   <- getMessageRender

  let maxMessageLength = boardMaxMsgLength  boardVal
      numberFiles      = boardNumberFiles   boardVal
      enableCaptcha    = boardEnableCaptcha boardVal
      forcedAnon       = boardEnableForcedAnon boardVal
      myMessageField = checkBool (not . tooLongMessage maxMessageLength)
                                 (MsgTooLongMessage maxMessageLength )
                                 textareaField
      urls :: [(Text, GoBackTo)]
      urls = [(msgrender MsgToThread, ToThread), (msgrender MsgToBoard, ToBoard), (msgrender MsgToFeed, ToFeed)]
      passInput    lbl = lbl { fsAttrs = [("autocomplete","off")] }
      captchaInput lbl = lbl { fsAttrs = [("placeholder",msgrender MsgCaptcha)] }
      msgInput     lbl = lbl { fsAttrs = [("placeholder",msgrender MsgMessage)] }
      nameInput    lbl = lbl { fsAttrs = [("autocomplete","off"),("maxlength",showText maxLenOfPostName ),("placeholder",msgrender MsgName)] }
      subjectInput lbl = lbl { fsAttrs = [("autocomplete","off"),("maxlength",showText maxLenOfPostTitle),("placeholder",msgrender MsgSubject)] }
  ----------------------------------------------------------------------------------------------------------------
  (nameRes     , nameView    ) <- mopt textField              (nameInput    "") (Just              <$> lastName)
  (subjectRes  , subjectView ) <- mopt textField              (subjectInput "") (Just              <$> lastTitle)
  (messageRes  , messageView ) <- mopt myMessageField         (msgInput     "") ((Just . Textarea) <$> lastMessage)
  (passwordRes , passwordView) <- mreq passwordField          (passInput    "") Nothing
  (captchaRes  , captchaView ) <- mopt textField              (captchaInput "") Nothing
  (gobackRes   , gobackView  ) <- mreq (selectFieldList urls)               ""  (Just $ maybe ToBoard (\x -> readText x :: GoBackTo) lastGoback)
  (nobumpRes   , nobumpView  ) <- mopt checkBoxField                        ""  Nothing
  (fileresults , fileviews   ) <- unzip <$> forM ([1..numberFiles] :: [Int]) (\_ -> mopt fileField "File" Nothing)
  let result = (,,,,,,,) <$>   nameRes <*> subjectRes <*> messageRes <*> captchaRes <*> passwordRes <*>
               FormSuccess fileresults <*> gobackRes  <*> nobumpRes
      widget = $(widgetFile "post-form")
  return (result, widget)
-------------------------------------------------------------------------------------------------------------------
editForm :: Html -> MForm Handler (FormResult (Textarea, Text, Int), Widget)
editForm extra = do
  (postIdRes  , postIdView  ) <- mreq intField      "" Nothing
  (messageRes , messageView ) <- mreq textareaField "" Nothing
  (passwordRes, passwordView) <- mreq passwordField "" Nothing  
  -- msgrender <- getMessageRender
  let result = (,,) <$> messageRes <*> passwordRes <*> postIdRes
      widget = $(widgetFile "edit-form")
  return (result, widget)
-------------------------------------------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------------------------------------------
isFileAllowed :: [String] -> FormResult (Maybe FileInfo) -> Bool
isFileAllowed allowedTypes (FormSuccess (Just x)) = fileExt x `elem` allowedTypes
isFileAllowed _            _                      = True

noMessage :: Maybe Textarea -> Bool
noMessage message = maybe True (T.null . T.filter (`notElem`" \r\n\t") . unTextarea) message

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
-------------------------------------------------------------------------------------------------------------------      
-- | If ajax request, redirects to page that makes JSON from message and status string.
--   If regular request, redirects to given URL.
trickyRedirect status msg url = do
  setMessageI msg
  t <- isAjaxRequest
  if t
    then redirect (JsonFromMsgR status)
    else redirect url
