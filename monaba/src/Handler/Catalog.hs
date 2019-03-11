module Handler.Catalog where

import           Import
import qualified Data.Text          as T
import           Handler.Posting
-------------------------------------------------------------------------------------------------------------------
getCatalogR :: Text -> Handler Html
getCatalogR board = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  boardVal <- getBoardVal404 board
  checkViewAccess mgroup boardVal
  let permissions = getPermissions mgroup
      title       = boardTitle   boardVal
      summary     = boardSummary boardVal
  -------------------------------------------------------------------------------------------------------
  posterId <- getPosterId
  hiddenThreads <- map fst <$> getHiddenThreads board
  let selectorHB  = [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False, PostLocalId /<-. hiddenThreads, PostHellbanned ==. False] ||.
                    [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False, PostLocalId /<-. hiddenThreads, PostHellbanned ==. True, PostPosterId ==. posterId]
      selectorAll = [PostBoard ==. board, PostParent ==. 0, PostDeleted ==. False, PostLocalId /<-. hiddenThreads, PostHellbanned ==. False] 
      selector    = if elem HellBanP permissions then selectorAll else selectorHB
      selectThreads = selectList selector [Desc PostBumped]

      selectFiles  pId = selectList [AttachedfileParentId ==. pId] []
      countPostsAll t = [PostDeletedByOp ==. False, PostDeleted ==. False, PostBoard ==. board, PostParent ==. postLocalId t]
      countPostsHB  t = [PostDeletedByOp ==. False, PostDeleted ==. False, PostBoard ==. board, PostParent ==. postLocalId t, PostHellbanned ==. False] ||. 
                        [PostDeletedByOp ==. False, PostDeleted ==. False, PostBoard ==. board, PostParent ==. postLocalId t, PostHellbanned ==. True, PostPosterId ==. posterId]
      countPosts t = if elem HellBanP permissions then countPostsAll t else countPostsHB t

  postsAndFiles <- runDB $ selectThreads >>= mapM (\th@(Entity tId t) -> do
                    threadFiles   <- selectFiles tId
                    postsInThread <- count (countPosts t)
                    return (th, threadFiles, postsInThread))

  AppSettings{..} <- appSettings <$> getYesod
  mBanner         <- randomBanner
  msgrender       <- getMessageRender
  defaultLayout $ do
    setUltDestCurrent
    defaultTitle $ T.concat [title, appTitleDelimiter, msgrender MsgCatalog]
    $(widgetFile "catalog")
