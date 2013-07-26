{-# LANGUAGE TupleSections, OverloadedStrings #-}
module AwfulMarkup
       (
         doAwfulMarkup
       ) where

import           Import
import           Prelude
import           Text.Blaze.Html.Renderer.String
import           Data.List.Split                   (splitOn)
import           Text.Regex.PCRE.Light.Extra       ((=~))
import           Text.Regex.PCRE.ByteString.Utils  (substituteCompile)
import qualified Data.ByteString.UTF8              as B
import qualified Data.ByteString                   as B hiding (take,drop,length)
import           System.Process
-------------------------------------------------------------------------------------------------------------------
geshi :: String
geshi = "./highlight.php"
php :: String
php = "/usr/bin/php"
-------------------------------------------------------------------------------------------------------------------
doAwfulMarkup :: Maybe Textarea -> Text -> Int -> Handler Textarea
doAwfulMarkup Nothing  _     _      = return $ Textarea ""
doAwfulMarkup (Just s) board thread = do
  let (sources, htmlWithoutSources) = cutSourceCodes $ unpack $ unTextarea s
  formattedSources <- liftIO $ mapM codeHighlight sources
  formattedText    <- mapM ((B.toString <$>) . doMarkup thread (B.fromString $ unpack board) . B.fromString . escapeHtml)
                          htmlWithoutSources
  return $ Textarea $ pack $ pasteSourceCodes (formattedSources, formattedText)
-------------------------------------------------------------------------------------------------------------------
-- regex replace  
-------------------------------------------------------------------------------------------------------------------
(=~$) :: B.ByteString -> (B.ByteString, B.ByteString) -> IO B.ByteString
(=~$) source' (from', to') = helper source' from' to' (B.fromString "")
  where helper source from to acc = do
          let maybeFound = source =~ from :: Maybe [B.ByteString]
          case maybeFound of
            Just found -> do
              let x     = head found -- full match
                  i     = B.length x + fromJust (B.findSubstring x source) -- suppose findSubstring always succeeds...
                  left  = B.take i source
                  right = B.drop i source
              result <- substituteCompile from left to
              case result of
                Right z   -> helper right from to (B.concat [acc, z])
                Left  err -> error $ "error at substituteCompile:" ++ err
            Nothing -> return $ B.concat [acc,source]
-------------------------------------------------------------------------------------------------------------------
-- kind of unfold2 function
cutSourceCodes :: String -> ([(String,String)],[String])
cutSourceCodes t = cutSourceCodes' t ([],[])
  where cutSourceCodes' x (sources, htmls) =
          case (B.fromString x =~ ("\\[code=(\\w+)\\]((?:.|\n)+?)\\[/code\\]"::B.ByteString) :: Maybe [B.ByteString]) of
            Just ( _ : lang : source : _) -> let c = "[code="++ B.toString lang ++"]"++ B.toString source ++"[/code]"
                                             in cutSourceCodes' (concat $ tail $ splitOn c x)
                                                ((B.toString lang, B.toString source):sources, head (splitOn c x):htmls)
            Nothing                       -> (sources, x:htmls)
            _                             -> error "bad regex at =~ "

-- and this is fold2
pasteSourceCodes :: ([(String,String)],[String]) -> String
pasteSourceCodes t = pasteSourceCodes' t ""
  where pasteSourceCodes' ( (_,source):sources, chunk:chunks) result = pasteSourceCodes' (sources,chunks) pasted
          where pasted = source++chunk++result
        pasteSourceCodes' ( [] , chunk:[]) result = chunk ++ result
        pasteSourceCodes' _                 _     = error "bad regex at =~"
-------------------------------------------------------------------------------------------------------------------
codeHighlight :: (String,String) -> IO (String, String)
codeHighlight (lang,source) = do
   s <- readProcess php [geshi, lang] source
   return (lang, s)

escapeHtml :: String -> String
escapeHtml = renderHtml . toHtml

doMarkup :: Int -> B.ByteString -> B.ByteString -> Handler B.ByteString
doMarkup thread board s = doProofLabels thread s board >>= (`doReflinks` board) >>= (\s' -> liftIO $ foldr (=<<) (clickableUrls s') allTags)
  where clickableUrls = (=~$ ("((?:https?|ftp|gopher)://[^(\\s<>\\[\\])]+)"  , "<a href='\\1'>\\1</a>"                   ))
        quotes        = (=~$ ("(?:(?:\n\r)|(?:\n))&gt;(.+)"   , "<br><span class='quote' style='color:green'>>\\1</span>"))
        quotes'       = (=~$ ("^&gt;(.+)"                     , "<span class='quote' style='color:green'>>\\1</span>"))
        newlines      = (=~$ ("(?:(?:\n\r)|(?:\n))+"          , "<br>"                                               ))
        bold          = (=~$ ("\\[b\\]((?:.|\n)+?)\\[/b\\]"   , "<strong>\\1</strong>"                               )) 
        bold'         = (=~$ ("\\*\\*((?:.|\n)+?)\\*\\*"      , "<strong>\\1</strong>"                               ))
        bold''        = (=~$ ("__((?:.|\n)+?)__"              , "<strong>\\1</strong>"                               ))
        underline     = (=~$ ("\\[u\\]((?:.|\n)+?)\\[/u\\]"   , "<u>\\1</u>"                                         ))
        italic        = (=~$ ("\\[i\\]((?:.|\n)+?)\\[/i\\]"   , "<em>\\1</em>"                                       ))
        italic'       = (=~$ ("_((?:.|\n)+?)_"                , "<em>\\1</em>"                                       ))
        italic''      = (=~$ ("\\*((?:.|\n)+?)\\*"            , "<em>\\1</em>"                                       ))
        strike        = (=~$ ("\\[s\\]((?:.|\n)+?)\\[/s\\]"   , "<s>\\1</s>"                                         ))
        spoiler'      = (=~$ ("%%((?:.|\n)+?)%%"              , B.concat [openSpoiler, "\\1", closeSpoiler]          ))
        spoiler       = (=~$ ("\\[spoiler\\]((?:.|\n)+?)\\[/spoiler\\]", B.concat [openSpoiler, "\\1", closeSpoiler] ))
        openSpoiler   = "<span class='spoiler' onmouseout=\"this.style.color='black'\" onmouseover=\"this.style.color='white';\" style=\"color:black; background-color:black\">"
        closeSpoiler  = "</span>"
        allTags       = [ newlines, spoiler, spoiler', underline, italic, italic', italic''
                        , strike, bold, bold', bold'', quotes, quotes']
-------------------------------------------------------------------------------------------------------------------
doReflinks :: B.ByteString -> B.ByteString -> Handler B.ByteString
doReflinks s' currentBoard = helper s' ""
  where regex = "((?:&gt;&gt;)|(?:##))(?:/?(\\w+)/)?(\\d+)" :: B.ByteString
        helper source acc = do
          let maybeFound = source =~ regex :: Maybe [B.ByteString]
          case maybeFound of
            Just found -> do
              let linkType = found !! 1
                  board' = found !! 2
                  postId = found !! 3
                  x      = head found -- full match
                  i      = B.length x + fromJust (B.findSubstring x source) -- suppose findSubstring always succeeds...
                  left   = B.take i source
                  right  = B.drop i source
                  isCrossBoard = not (B.null board')
                  board  = if isCrossBoard then board' else currentBoard
              result <- replaceLink regex left board isCrossBoard postId linkType
              case result of
                Right z   -> helper right (B.concat [acc, z])
                Left  err -> error $ "error at substituteCompile:" ++ err
            Nothing -> return $ B.concat [acc,source]

replaceLink :: B.ByteString -> -- regex
              B.ByteString -> -- source string
              B.ByteString -> -- found board
              Bool         -> -- is a cross board link
              B.ByteString -> -- post id
              B.ByteString -> -- regular link or proof label — &gt;&gt; or ##
              Handler (Either String B.ByteString)
replaceLink regex source board isCrossBoard postId linkType' = do
  maybePost <- runDB $ selectFirst [PostLocalId ==. read (B.toString postId), PostBoard ==. pack (B.toString board)] []
  case maybePost of
    Just post -> do
      let linkType       = if B.toString linkType' == "&gt;&gt;" then ">>" else "##"
          localBoard thr = B.concat ["<a onmouseover='timeout(this, function(){showPopupPost(event, this,\"",board,"\",",postId,")},700)' onclick='highlightPost(\"post-",postId,"-",thr,"-",board,"\")' href='/thread/",board,"/",thr,"#",postId,"'>",linkType,postId,"</a>"]
          crossBoard thr = B.concat ["<a onmouseover='timeout(this, function(){showPopupPost(event, this,\"",board,"\",",postId,")},700)' onclick='highlightPost(\"post-",postId,"-",thr,"-",board,"\")' href='/thread/",board,"/",thr,"#",postId,"'>",linkType,"/",board,"/",postId,"</a>"]
          parent'        = postParent $ entityVal post
          parent         = if parent' == 0  then postId     else B.fromString $ show parent'
          f              = if isCrossBoard then crossBoard else localBoard
      liftIO $ substituteCompile regex source (f parent)
    Nothing -> return (Right source)
-------------------------------------------------------------------------------------------------------------------
doProofLabels :: Int -> B.ByteString -> B.ByteString -> Handler B.ByteString
doProofLabels thread s' board = helper s' ""
  where regex = "##((?:\\d+)|(?:OP))" :: B.ByteString
        helper source acc = do
          let maybeFound = source =~ regex :: Maybe [B.ByteString]
          case maybeFound of
            Just found -> do
              let postId = found !! 1
                  x      = head found -- full match
                  i      = B.length x + fromJust (B.findSubstring x source) -- suppose findSubstring always succeeds...
                  left   = B.take i source
                  right  = B.drop i source
              result <- replaceProofLabel regex left board (B.toString postId) thread
              case result of
                Right z   -> helper right (B.concat [acc, z])
                Left  err -> error $ "error at substituteCompile:" ++ err
            Nothing -> return $ B.concat [acc,source]

replaceProofLabel :: B.ByteString -> -- regex
                    B.ByteString -> -- source string
                    B.ByteString -> -- board
                    String       -> -- post id
                    Int          -> -- thread local id
                    Handler (Either String B.ByteString)
replaceProofLabel regex source board postId' thread = do
  let postId = if postId' == "OP" then thread else read postId'
  posterId  <- getPosterId
  maybePost <- runDB $ selectFirst [PostBoard ==. pack (B.toString board), PostLocalId ==. postId] []
  case maybePost of
    Nothing -> return (Right source)
    Just (Entity _ post) -> let spanClass = if posterId == postPosterId post then "pLabelTrue" else "pLabelFalse"
                            in liftIO $ substituteCompile regex source (B.concat ["<span class='", spanClass, "'>##\\1</span>"])
