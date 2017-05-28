{-# LANGUAGE ScopedTypeVariables #-}
module Handler.API where

import Import
import Data.Aeson
import Handler.Thread
import Handler.Board
import Handler.Feed
import Handler.Catalog
import Handler.Home
import Handler.Ajax

getApiThreadR  = getThreadR
getApiBoardR   = getBoardR
getApiFeedR    = getAjaxFeedOffsetR
getApiCatalogR = getCatalogR
getApiHomeR    = getHomeR

getApiListBoardsR :: Handler TypedContent
getApiListBoardsR = do
  group  <- (fmap $ userGroup . entityVal) <$> maybeAuth
  boards <- runDB $ selectList ([]::[Filter Board]) []
  config <- getConfigEntity
  let boardCategories = configBoardCategories config ++ [""]
      filteredBoards  = object $ map (\c -> c .= filterBoards boards c group) boardCategories
  selectRep $ do
    provideJson $ filteredBoards
    
getApiPostByIdR = getAjaxPostByIdR

putApiPostR :: Handler ()
putApiPostR = do
  -- req <- (requireJsonBody :: Handler PostRequest)

  -- when (thread <= 0) $   sendResponseStatus status404 ("NOT FOUND" :: Text)
  -- muser    <- maybeAuth
  -- mgroup   <- getMaybeGroup muser

  -- mboard    <- runDB (getBy $ BoardUniqName board)
  -- when (isNothing mboard) $ sendResponseStatus status404 ("NOT FOUND" :: Text)

  -- let boardVal         = entityVal $ fromJust mboard
  --     permissions      = getPermissions mgroup
  --     defaultName      = boardDefaultName      boardVal
  --     allowedTypes     = boardAllowedTypes     boardVal
  --     thumbSize        = boardThumbSize        boardVal
  --     bumpLimit        = boardBumpLimit        boardVal
  --     replyFile        = boardReplyFile        boardVal
  --     enableCaptcha    = boardEnableCaptcha    boardVal
  --     forcedAnon       = boardEnableForcedAnon boardVal
  --     enablePM         = boardEnablePM         boardVal

  -- hasAccess <- checkViewAccess' mgroup boardVal
  -- unless hasAccess $ sendResponseStatus status404 ("NOT FOUND" :: Text)

  -- msgrender <- getMessageRender
  -- maybeParent <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  -- when (isNothing maybeParent) $
  --   sendStatusJSON status404 $ object ["error" .= True, "message" .= (toJSON (msgrender MsgNoSuchThread :: Text)) ]
  sendResponseStatus status201 ("CREATED" :: Text)
