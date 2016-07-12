module Handler.Edit where
 
import           Import
import           Handler.Posting
import           Handler.EventSource (sendEditedPostES)
import           Utils.YobaMarkup (doYobaMarkup)
import qualified Data.Text as T (intercalate)
-------------------------------------------------------------------------------------------------------------------
postPostEditR :: Handler TypedContent
postPostEditR = do
  muser       <- maybeAuth
  permissions <- getPermissions <$> getMaybeGroup muser
  ((result, _), _) <- runFormPost $ editForm permissions
  case result of
    FormFailure []                  -> trickyRedirect "error" (Left MsgBadFormData) HomeR
    FormFailure xs                  -> trickyRedirect "error" (Left $ MsgError $ T.intercalate "; " xs) HomeR
    FormMissing                     -> trickyRedirect "error" (Left MsgNoFormData)  HomeR
    FormSuccess (newMessage, pswd, postId, mShadowEdit) -> do
      let postKey = (toSqlKey . fromIntegral) postId :: Key Post
          shadowEdit = fromMaybe False mShadowEdit
      post     <- runDB (get404 postKey)
      posterId <- getPosterId
      boardVal <- getBoardVal404 (postBoard post)
      maxTimes <- getConfig configMaxEditings

      unless (EditPostsP `elem` permissions) $ do
        when (postParent post == 0 && not (boardOpEditing   boardVal)) $
          trickyRedirect "error" (Left MsgThreadEditingIsDisabled) HomeR
        when (postParent post /= 0 && not (boardPostEditing boardVal)) $
          trickyRedirect "error" (Left MsgPostEditingIsDisabled)   HomeR
        when (postLockEditing post) $ 
          trickyRedirect "error" (Left MsgDisabledEditing) HomeR
  
        when (postPosterId post /= posterId &&
              postPassword post /= pswd
             ) $ trickyRedirect "error" (Left MsgPostNotYours) HomeR

      let maxMessageLength = boardMaxMsgLength boardVal
        in when (tooLongMessage maxMessageLength newMessage) $
            trickyRedirect "error" (Left $ MsgTooLongMessage maxMessageLength) HomeR

      checkWordfilter (Just newMessage) (postBoard post) $ \m -> trickyRedirect "error" m HomeR

      filteredMsg <- lookupSession "filtered-message"
      messageFormatted <- doYobaMarkup (maybe (Just newMessage) (Just . Textarea) filteredMsg) (postBoard post) (postParent post)
      history <- runDB $ getBy $ HistoryUniqPostId postKey
      unless (EditPostsP `elem` permissions) $ 
        let z = maybe 0 (length . historyMessages . entityVal) history
          in when (z >= maxTimes) $ trickyRedirect "error" (Left $ MsgYouAlreadyEditedPost maxTimes) HomeR
        
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
                             , PostRawMessage =. fromMaybe (unTextarea newMessage) filteredMsg]
                             ++[PostLastModified =. Just now | not (ShadowEditP `elem` permissions && shadowEdit)])
      unless (ShadowEditP `elem` permissions && shadowEdit) $
        if isJust history
          then runDB $ replace (entityKey $ fromJust history) newHistory
          else void $ runDB $ insert newHistory
      sendEditedPostES postId
      trickyRedirect "ok" (Left MsgPostEdited) HomeR
  
getEditHistoryR :: Int -> Handler Html
getEditHistoryR postId = do
  let postKey = (toSqlKey . fromIntegral) postId :: Key Post
  h     <- runDB $ getBy404 $ HistoryUniqPostId postKey
  post  <- runDB $ get404 postKey
  board <- getBoardVal404 (postBoard post)
  unless (boardShowEditHistory board) $ do
    setMessageI MsgEditingHistoryIsDisabled
    redirectUltDest (BoardNoPageR $ boardName board)

  let history = reverse $ zip (historyDates $ entityVal h) (historyMessages $ entityVal h)

  timeZone <- getTimeZone
  defaultLayout $ do
    setUltDestCurrent
    defaultTitleMsg MsgEditingHistory
    $(widgetFile "edit-history")
