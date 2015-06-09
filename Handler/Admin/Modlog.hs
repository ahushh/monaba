module Handler.Admin.Modlog where

import Import

getModlogR :: Int -> Handler Html
getModlogR page = do
  logEntryCount  <- runDB $ count ([]::[Filter Modlog])
  perPage        <- getConfig configModlogEntriesPerPage
  entries        <- runDB $ selectList ([]::[Filter Modlog]) [Desc ModlogDate, LimitTo perPage, OffsetBy $ page*perPage]
  timeZone       <- getTimeZone
  let pages = listPages perPage logEntryCount
  defaultLayout $ do
    defaultTitleMsg MsgModlog
    $(widgetFile "admin/modlog")

addModlogEntry :: AppMessage -> Handler ()
addModlogEntry msg = do
  muser     <- maybeAuth
  mgroup    <- getMaybeGroup muser
  when (isNothing muser || isNothing mgroup) notFound
  msgrender <- getMessageRender
  now       <- liftIO getCurrentTime
  let entry = Modlog { modlogUser    = userName  (entityVal $ fromJust muser )
                     , modlogGroup   = groupName (entityVal $ fromJust mgroup)
                     , modlogDate    = now
                     , modlogMessage = msgrender msg
                     }
  void $ runDB $ insert entry
  maxEntries <- getConfig configModlogMaxEntries
  toDelete <- runDB $ selectList ([]::[Filter Modlog]) [Desc ModlogDate, OffsetBy maxEntries]
  runDB $ forM_ toDelete $ \(Entity k _) -> delete k
