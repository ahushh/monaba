module Handler.Search where

import           Import
import           Data.Conduit
import qualified Data.Conduit.List                       as CL
import qualified Data.XML.Types                          as X
import qualified Text.Search.Sphinx                      as S
import qualified Text.Search.Sphinx.ExcerptConfiguration as E
import qualified Text.Search.Sphinx.Types                as ST
import           Text.XML.Stream.Render                  (def, renderBuilder)
import           Text.HTML.TagSoup  (escapeHTML)
--------------------------------------------------------------------------------------------------------- 
searchLimit :: Int
searchLimit = 50

getSearchR :: Handler Html
getSearchR = do
  ((formRes, searchWidget), _) <- runFormGet $ searchForm Nothing
  let offset = 0
  case formRes of
    FormSuccess (qstring, mBoard) -> do
      searchResults <- getResults qstring mBoard 0
      let board = fromMaybe "" mBoard
      defaultLayout $ do
        defaultTitleMsg MsgSearch
        $(widgetFile "search")
    _ -> do
      let searchResults = []
          qstring       = ""
          board         = ""
      defaultLayout $ do
        defaultTitleMsg MsgSearch
        $(widgetFile "search")

getSearchMoreR :: Text -> Text -> Int -> Handler Html
getSearchMoreR qstring board offset = do
  searchResults <- getResults qstring (Just board) offset
  defaultLayout $ do
    defaultTitleMsg MsgSearch
    $(widgetFile "search")

getResults :: Text -> Maybe Text -> Int -> Handler [SearchResult]
getResults qstring board offset = do
  muser    <- maybeAuth
  mgroup   <- getMaybeGroup muser
  let permissions = getPermissions mgroup
      group       = (groupName . entityVal) <$> mgroup
  posterId <- getPosterId
  boards   <- runDB $ selectList ([]::[Filter Board]) []
  let ignoredBoards  = mapMaybe (getIgnoredBoard group) boards
      checkBoard   p = (isJust board && (fromJust board == postBoard p || fromJust board == "")) || isNothing board
      checkAccess  p = postBoard p `notElem` ignoredBoards
      checkDeleted p = not $ postDeleted p
      checkPM      p = (isNothing $ postDestUID p) || (isJust (postDestUID p) && fromJust (postDestUID p) == posterId)
      checkHB      p = not (postHellbanned p) || (postHellbanned p || postPosterId p == posterId) || elem HellBanP permissions
      checkAll     p = checkBoard p && checkAccess p && checkDeleted p && checkPM p && checkHB p

  sphinxRes' <- liftIO $ S.query config "monaba" qstring
  case sphinxRes' of
        ST.Ok sphinxRes -> do
            let postIds = map (toSqlKey . ST.documentId) $ ST.matches sphinxRes
            posts <- fmap (filter checkAll . catMaybes) $ runDB $ forM postIds get
            forM (zip postIds posts) $ \(postId, post) -> liftIO $ getResult postId post qstring
        _ -> error $ show sphinxRes'
  where
    config = S.defaultConfig
        { S.port   = 9312
        , S.mode   = ST.Extended
        , S.offset = offset
        , S.limit  = searchLimit
        }

getResult :: PostId -> Post -> Text -> IO SearchResult
getResult postId post qstring = do
    excerpt' <- S.buildExcerpts
        excerptConfig
        [escapeHTML $ postRawMessage post]
        "monaba"
        qstring
    let excerpt =
            case excerpt' of
                ST.Ok texts -> preEscapedToHtml $ mconcat texts
                _ -> ""
      in return SearchResult
        { searchResultPostId  = postId
        , searchResultPost    = post
        , searchResultExcerpt = excerpt
        }
  where
    excerptConfig = E.altConfig { E.port = 9312
                                , E.beforeMatch = "<span class='match'>"
                                , E.afterMatch = "</span>"
                                , E.around = 10
                                , E.forceAllWords = False
                                }
--------------------------------------------------------------------------------------------------------- 
getXmlpipeR :: Handler TypedContent
getXmlpipeR =
    respondSourceDB "text/xml"
 $  fullDocSource
 $= renderBuilder def
 $= CL.map Chunk

fullDocSource :: Source (YesodDB App) X.Event
fullDocSource = do
    mapM_ yield startEvents
    docSource -- $= CL.filter (\_ -> True)
    mapM_ yield endEvents

startEvents, endEvents :: [X.Event]
startEvents =
    [ X.EventBeginDocument
    , X.EventBeginElement docset []
    , X.EventBeginElement schema []
    , X.EventBeginElement field [("name", [X.ContentText "content"])]
    , X.EventEndElement field
    , X.EventEndElement schema
    ]

endEvents =
    [ X.EventEndElement docset
    ]

docSource :: Source (YesodDB App) X.Event
docSource = selectSource [] [] $= CL.concatMap entityToEvents

entityToEvents :: (Entity Post) -> [X.Event]
entityToEvents (Entity postId post) =
    [ X.EventBeginElement document [("id", [X.ContentText $ toPathPiece postId])]
    , X.EventBeginElement content []
    , X.EventContent $ X.ContentText $ postRawMessage post
    , X.EventEndElement content
    , X.EventEndElement document
    ]

toName :: Text -> X.Name
toName x = X.Name x (Just "http://sphinxsearch.com/") (Just "sphinx")

docset, schema, field, document, content :: X.Name
docset = toName "docset"
schema = toName "schema"
field = toName "field"
document = toName "document"
content = "content" -- no prefix

