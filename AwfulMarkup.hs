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
geshi = "/home/user/geshi/highlight.php"
php :: String
php = "/usr/bin/php"
-------------------------------------------------------------------------------------------------------------------
doAwfulMarkup :: Maybe Textarea -> Text -> Handler Textarea
doAwfulMarkup Nothing  _      = return $ Textarea ""
doAwfulMarkup (Just s) board  = do
  let (sources, htmlWithoutSources) = cutSourceCodes $ unpack $ unTextarea s
  formattedSources <- liftIO $ mapM codeHighlight sources
  escapedHtmls     <- mapM ((B.toString <$>) . doTags (B.fromString $ unpack board) . B.fromString . escapeHtml) htmlWithoutSources
  return $ Textarea $ pack $ pasteSourceCodes (formattedSources, escapedHtmls)
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

doTags :: B.ByteString -> B.ByteString -> Handler B.ByteString
doTags board s = doReflinks s board >>= (\s' -> liftIO $ foldr (=<<) (clickableUrls s') allTags)
  where clickableUrls = (=~$ ("(https?://[^(?\n<>\\[\\])]+)"  , "<a href='\\1'>\\1</a>"                              ))
        quotes        = (=~$ ("&gt;([^\n]+)"                  , "<span class='quote' style='color:green'>>\\1</span>"))
        newlines      = (=~$ ("(\n|\r)+"                      , "<br>"                                               ))
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
        openSpoiler   = "<span onmouseout=\"this.style.color='black'\" onmouseover=\"this.style.color='white';\" style=\"color:black; background-color:black\">"
        closeSpoiler  = "</span>"
        allTags       = [newlines, spoiler, spoiler', underline, italic, italic', italic'', strike, bold, bold', bold'', quotes]

doReflinks :: B.ByteString -> B.ByteString -> Handler B.ByteString
doReflinks s' currentBoard = helper s' (B.fromString "")
  where regex = "&gt;&gt;(?:/?(\\w+)/)?(\\d+)" :: B.ByteString
        helper source acc = do
          let maybeFound = source =~ regex :: Maybe [B.ByteString]
          case maybeFound of
            Just found -> do
              let board' = found !! 1
                  postId = found !! 2
                  x      = head found -- full match
                  i      = B.length x + fromJust (B.findSubstring x source) -- suppose findSubstring always succeeds...
                  left   = B.take i source
                  right  = B.drop i source
                  isCrossBoard = not (B.null board')
                  board  = if isCrossBoard then board' else currentBoard
              result <- replaceLink regex left board isCrossBoard postId
              case result of
                Right z   -> helper right (B.concat [acc, z])
                Left  err -> error $ "error at substituteCompile:" ++ err
            Nothing -> return $ B.concat [acc,source]

replaceLink :: B.ByteString -> -- regex
              B.ByteString -> -- source string
              B.ByteString -> -- found board
              Bool         -> -- is a cross board link
              B.ByteString -> -- post id
              Handler (Either String B.ByteString)
replaceLink regex source board isCrossBoard postId = do
  maybePost <- runDB $ selectFirst [PostLocalId ==. read (B.toString postId), PostBoard ==. pack (B.toString board)] []
  case maybePost of
    Just post -> do
      let localBoard thr = B.concat ["<a href='/thread/", board, "/", thr, "/#", postId, "'>>>" , postId, "</a>"]
          crossBoard thr = B.concat ["<a href='/thread/", board, "/", thr, "/#", postId, "'>>>/", board , "/", postId, "</a>"]
          parent'        = postParent $ entityVal post
          parent         = if parent' == 0  then postId     else B.fromString $ show parent'
          f              = if isCrossBoard then crossBoard else localBoard
      liftIO $ substituteCompile regex source (f parent)
    Nothing -> return (Right source)
