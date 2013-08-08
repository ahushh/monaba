{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Edit where
 
import           Import
import qualified Data.Text       as T
import           Yesod.Auth
import           Handler.Posting
import           AwfulMarkup        (doAwfulMarkup)
-------------------------------------------------------------------------------------------------------------------
postPostEditR :: Handler TypedContent
postPostEditR = do
  muser  <- maybeAuth
  mgroup <- getMaybeGroup muser
  let permissions = getPermissions mgroup
  ((result, _), _) <- runFormPost editForm
  case result of
    FormFailure _                   -> trickyRedirect "error" MsgBadFormData HomeR
    FormMissing                     -> trickyRedirect "error" MsgNoFormData  HomeR
    FormSuccess (newMessage, pswd, postId) -> do
      let postKey = toKey postId :: Key Post
      post     <- runDB (get404 postKey)
      posterId <- getPosterId
      boardVal <- getBoardVal404 (postBoard post)
      maxTimes <- getConfig configMaxEditings

      unless (EditPostsP `elem` permissions) $ do
        when ((postParent post) == 0 && not (boardOpEditing   boardVal)) $
          trickyRedirect "error" MsgThreadEditingIsDisabled HomeR
        when ((postParent post) /= 0 && not (boardPostEditing boardVal)) $
          trickyRedirect "error" MsgPostEditingIsDisabled   HomeR
  
        when (postPosterId post /= posterId &&
              postPassword post /= pswd
             ) $ trickyRedirect "error" MsgPostNotYours HomeR

      messageFormatted <- doAwfulMarkup (Just newMessage) (postBoard post) (postParent post)
      history <- runDB $ getBy $ HistoryUniqPostId postKey
      unless (EditPostsP `elem` permissions) $ 
        let z = maybe 0 (length . historyMessages . entityVal) history
          in when (z >= maxTimes) $ trickyRedirect "error" (MsgYouAlreadyEditedPost maxTimes) HomeR
        
      now     <- liftIO getCurrentTime
      let oldMessage = postMessage post
          oldDate    = fromMaybe (postDate post) (postLastModified post)
          oldMessages= maybe [] (historyMessages . entityVal) history
          oldDates   = maybe [] (historyDates    . entityVal) history
          newHistory = History { historyPostId   = postKey
                               , historyMessages = oldMessage : oldMessages
                               , historyDates    = oldDate    : oldDates
                               }
      runDB $ update postKey [PostMessage =. messageFormatted, PostLastModified =. Just now]
      if isJust history
        then runDB $ replace (entityKey $ fromJust history) newHistory
        else void $ runDB $ insert newHistory
      trickyRedirect "ok" MsgPostEdited HomeR
  
getEditHistoryR :: Int -> Handler Html
getEditHistoryR postId = do
  let postKey = toKey postId :: Key Post
  h     <- runDB $ getBy404 $ HistoryUniqPostId postKey
  post  <- runDB $ get404 postKey
  board <- getBoardVal404 (postBoard post)
  unless (boardShowEditHistory board) $ do
    setMessageI MsgEditingHistoryIsDisabled
    redirectUltDest (BoardNoPageR $ boardName board)

  let history = reverse $ zip (historyDates $ entityVal h) (historyMessages $ entityVal h)

  nameOfTheBoard   <- extraSiteName <$> getExtra
  msgrender        <- getMessageRender
  defaultLayout $ do
    setUltDestCurrent
    setTitle $ toHtml $ T.concat [nameOfTheBoard, " — ", msgrender MsgEditingHistory]
    $(widgetFile "edit-history")
