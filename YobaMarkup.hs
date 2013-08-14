{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module YobaMarkup
       (
         doYobaMarkup
       ) where

import           Import
import           Prelude
import           Yesod.Auth
import           Text.HTML.TagSoup  (escapeHTML)
import           Text.Parsec hiding (newline)
import           Text.Parsec.Text
import           System.Process
import           Control.Monad      (foldM)
import qualified Data.Text     as T (concat, append)
-------------------------------------------------------------------------------------------------------------------
type CodeLang = Text
data Expr = Bold          [Expr] -- [b]bold[/b]
          | Italic        [Expr] -- [i]italic[/i]
          | Underline     [Expr] -- [u]underline[/u]
          | Strike        [Expr] -- [s]strike[/s]
          | Spoiler       [Expr] -- [spoiler]spoiler[/spoiler]
          | SpoilerWakaba [Expr] -- %%spoiler%%
          | Code CodeLang Text   -- [code=lang]main = print "hi"[/code]
          | Quote         [Expr] -- >quoted
          | InnerRef      Int    -- >>1308
          | ExternalRef Text Int -- >>/b/1632
          | ProofLabel    Int    -- ##1717
          | ProofLabelOP         -- ##OP
          | Color  Text   [Expr] -- [color=red]blah-blah[/color]
          | GroupProof           -- #group
          | UserProof            -- #user
          | Link          Text   -- http://russia3.ru
          | Plain         Text   -- any text 
          | Newline              -- \n
          deriving (Show)
-------------------------------------------------------------------------------------------------------------------
geshi :: String
geshi = "./highlight.php"
php :: String
php = "/usr/bin/php"
-------------------------------------------------------------------------------------------------------------------
doYobaMarkup :: Maybe Textarea -> Text -> Int -> Handler Textarea
doYobaMarkup Nothing  _     _      = return $ Textarea ""
doYobaMarkup (Just s) board thread = do
  let parsed = parseMarkup $ unTextarea s
  case parsed of
    Right xs -> processMarkup xs board thread
    -- Right xs -> Textarea . (T.concat [escapeHTML (pack $ show xs),"<br>"]`T.append`) . unTextarea <$> processMarkup xs board thread    
    Left err -> return $ Textarea $ pack $ show err
-------------------------------------------------------------------------------------------------------------------
-- Processing
-------------------------------------------------------------------------------------------------------------------    
codeHighlight :: Text -> Text -> IO Text
codeHighlight lang source = pack <$> readProcess php [geshi, unpack lang] (unpack source)

processMarkup :: [Expr] -> Text -> Int -> Handler Textarea
processMarkup xs board thread = Textarea <$> foldM f "" xs
  where
    ----------------------------------------------------------------------------------------------------------
    -- Just helpers
    ----------------------------------------------------------------------------------------------------------
    openSpoiler = "<span class='spoiler' onmouseout=\"this.style.color='black'\" onmouseover=\"this.style.color='white';\">"
    refHtml acc brd thr p ref = T.concat [acc , "<a onmouseover='timeout(this, function(){showPopupPost(event, this,\""
                                         ,brd , "\",", p   , ")},700)' onclick='highlightPost(\"post-", p, "-"
                                         ,thr , "-"  , brd , "\")' href='/thread/", brd, "/"
                                         ,thr , "#"  , p   , "'>", ref            , "</a>"
                                         ]
    getUserName  = userName  . entityVal . fromJust
    getGroupName = groupName . entityVal . fromJust
    ----------------------------------------------------------------------------------------------------------
    boldHandler      acc x = (\g -> T.concat [acc, "<strong>" , g, "</strong>"]) <$> foldM f "" x
    italicHandler    acc x = (\g -> T.concat [acc, "<em>"     , g, "</em>"    ]) <$> foldM f "" x
    spoilerHandler   acc x = (\g -> T.concat [acc, openSpoiler, g, "</span>"  ]) <$> foldM f "" x
    strikeHandler    acc x = (\g -> T.concat [acc, "<s>"      , g, "</s>"     ]) <$> foldM f "" x
    underlineHandler acc x = (\g -> T.concat [acc, "<u>"      , g, "</u>"     ]) <$> foldM f "" x
    quoteHandler     acc x = (\g -> T.concat [acc, "<span class=quote>>", g ,"</span><br>"]) <$> foldM f "" x
    ----------------------------------------------------------------------------------------------------------
    colorHandler acc color' msg = do
      muser  <- maybeAuth
      mgroup <- getMaybeGroup muser
      if isNothing muser || isNothing mgroup || (AdditionalMarkupP `notElem` getPermissions mgroup)
        then (\g -> T.concat [acc, "[color=            ", color', "]" , g ,"[/color]"]) <$> foldM f "" msg
        else (\g -> T.concat [acc, "<span style='color:", color', "'>", g ,"</span>" ]) <$> foldM f "" msg
    ----------------------------------------------------------------------------------------------------------
    userproofHandler acc = do
      muser  <- maybeAuth
      mgroup <- getMaybeGroup muser
      if isNothing muser || isNothing mgroup || (AdditionalMarkupP `notElem` getPermissions mgroup)
        then return $ T.append acc "#user"
        else return $ T.concat [acc, "<span style='border-bottom: 1px red dashed'>#", getUserName muser , "</span>"]
    ----------------------------------------------------------------------------------------------------------
    groupproofHandler acc = do
      muser  <- maybeAuth
      mgroup <- getMaybeGroup muser
      if isNothing muser || isNothing mgroup || (AdditionalMarkupP `notElem` getPermissions mgroup)
        then return $ T.append acc "#group"
        else return $ T.concat [acc, "<span style='border-bottom: 1px red dotted'>#", getGroupName mgroup, "</span>"]
    ----------------------------------------------------------------------------------------------------------
    -- Function that process each Expr
    ----------------------------------------------------------------------------------------------------------
    f acc (Code  lang code') = T.append acc <$> liftIO (codeHighlight lang code')
    f acc (Plain          x) = return $ T.append acc $ escapeHTML x
    f acc (Color color' msg) = colorHandler      acc color' msg
    f acc (Bold           x) = boldHandler       acc x
    f acc (Italic         x) = italicHandler     acc x
    f acc (Underline      x) = underlineHandler  acc x
    f acc (Strike         x) = strikeHandler     acc x
    f acc (Spoiler        x) = spoilerHandler    acc x
    f acc (SpoilerWakaba  x) = spoilerHandler    acc x
    f acc (Quote          x) = quoteHandler      acc x
    f acc GroupProof         = groupproofHandler acc
    f acc UserProof          = userproofHandler  acc
    f acc (Link           x) = return $ T.concat [acc, "<a href='", x, "'>", x, "</a>"]
    f acc Newline            = return $ T.append acc "<br>"
    ----------------------------------------------------------------------------------------------------------
    f acc (InnerRef postId) = do
      maybePost <- runDB $ selectFirst [PostLocalId ==. postId, PostBoard ==. board] []
      let p = pack $ show postId
      case maybePost of
        Just (Entity _ pVal) -> do
          let parent = pack $ show $ postParent pVal
          return $ refHtml acc board parent p (T.append ">>" p)
        Nothing              -> return $ T.concat [acc, ">>", p]
    ----------------------------------------------------------------------------------------------------------
    f acc (ExternalRef board' postId) = do
      maybePost <- runDB $ selectFirst [PostLocalId ==. postId, PostBoard ==. board'] []
      let p = pack $ show postId
      case maybePost of
        Just (Entity _ pVal) -> do
          let parent = pack $ show $ postParent pVal
          return $ refHtml acc board' parent p (T.concat [">>/", board', "/", p])
        Nothing              -> return $ T.concat [acc, ">>", p]
    ----------------------------------------------------------------------------------------------------------
    f acc (ProofLabel    postId) = do
      posterId  <- getPosterId
      maybePost <- runDB $ selectFirst [PostLocalId ==. postId, PostBoard ==. board] []
      let p = pack $ show postId
      case maybePost of
        Just (Entity _ pVal) -> do
          let posterId' = postPosterId pVal
              parent    = pack $ show (postParent pVal)
              spanClass = if posterId == posterId' then "pLabelTrue" else "pLabelFalse"
              link'     = refHtml "" board parent p (T.append "##" p)
          return $ T.concat [acc, "<span class='", spanClass, "'>", link', "</span>"]
        Nothing              -> return $ T.concat [acc, "##", p]
    ----------------------------------------------------------------------------------------------------------
    f acc ProofLabelOP = do
      posterId  <- getPosterId
      maybePost <- runDB $ selectFirst [PostLocalId ==. thread, PostBoard ==. board] []
      let t = pack $ show thread
      case maybePost of
        Just (Entity _ pVal) -> do
          let posterId' = postPosterId pVal
              parent    = pack $ show (postParent pVal)
              spanClass = if posterId == posterId' then "pLabelTrue" else "pLabelFalse"
              link'     = refHtml "" board parent t "##OP"
          return $ T.concat [acc, "<span class='", spanClass, "'>", link', "</span>"]
        Nothing              -> return $ T.concat [acc, "##OP"]
    ----------------------------------------------------------------------------------------------------------
    -- f acc z               = return $ T.concat [acc, "==", pack (show z), "=="]
-------------------------------------------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------------------------------------------    
plain :: Parser Expr
plain = Plain . pack <$> many1 (myCheck >> myCheck' >> anyChar)
  where myCheck   = foldr (\f acc -> acc >> notFollowedBy (try f))
                          (notFollowedBy $ try $ string "[/b]"  )
                          tagEnds
        tagEnds   = map string ["[/i]","[/s]","[/u]","[/spoiler]","[/color]","%%"]
        myCheck'  = foldr (\f acc -> acc >> notFollowedBy (try f))
                          (notFollowedBy $ try quote)
                          wholeTags
        wholeTags = [ spoilerwakaba, newline, link, bold, italic, underline, strike, spoiler,
                      color, code, extref, innerref, proof, proofop, userproof, groupproof]
--------------------------------------------------------------
-- Kusaba-like tags
--------------------------------------------------------------
bold      :: Parsec Text () Expr
italic    :: Parsec Text () Expr
underline :: Parsec Text () Expr
strike    :: Parsec Text () Expr
spoiler   :: Parsec Text () Expr
bold      = tag Bold      "b"
italic    = tag Italic    "i"
underline = tag Underline "u"
strike    = tag Strike    "s"
spoiler   = tag Spoiler   "spoiler"

code :: Parsec Text () Expr
code = do
  void $ optional $ try newline'
  void $ string "[code="
  lang <- many1 $ noneOf "]"
  void $ char ']'
  content <- manyTill anyChar $ try (string "[/code]")
  void $ optional $ try newline'
  return $ Code (escapeHTML $ pack lang) (pack content)

color :: Parsec Text () Expr
color = do
  void $ string "[color="
  color'  <- many1 $ noneOf "]"
  void $ char ']'
  content <- many expr
  void $ string "[/color]"
  return $ Color (escapeHTML $ pack color') content

tag :: ([Expr] -> Expr) -> String -> Parsec Text () Expr
tag f name = do
  void $ string $ "["++name++"]"
  content <- many expr
  void $ string $ "[/"++name++"]"
  return $ f content

tryTags :: Parsec Text () Expr
tryTags = try bold
      <|> try italic
      <|> try underline
      <|> try strike
      <|> try spoiler
      <|> try code
      <|> try color
--------------------------------------------------------------
-- Wakaba-like tags
--------------------------------------------------------------
spoilerwakaba :: Parsec Text () Expr
spoilerwakaba = tagWakaba SpoilerWakaba "%%"

tagWakaba :: ([Expr] -> Expr) -> String -> Parsec Text () Expr
tagWakaba f name = do
  void $ string name 
  content <- many expr
  void $ string name
  return $ f content

tryWakabaTags :: Parsec Text () Expr
tryWakabaTags = try spoilerwakaba
--------------------------------------------------------------
-- Other
--------------------------------------------------------------
newline' :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m String
newline' = try (string "\n\r")
       <|> try (string "\r\n")
       <|> try (string "\r"  )
       <|> try (string "\n"  )

urlScheme :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m String
urlScheme = try (string "https" )
        <|> try (string "http"  )
        <|> try (string "ftp"   )
        <|> try (string "gopher")

link :: Parser Expr
link = do
  scheme <- urlScheme
  void $ string "://"
  rest   <- many1 $ noneOf " \n\t\r["
  return $ Link (T.concat [pack scheme, "://", escapeHTML (pack rest)])

quote :: Parser Expr
quote = char '>' >> Quote <$> manyTill allmostAllExpr (newline' <|> (eof >> return ""))
  where
    allmostAllExpr = try bold
                 <|> try italic
                 <|> try underline
                 <|> try strike
                 <|> try spoiler      
                 <|> try color    
                 <|> tryWakabaTags
                 <|> try link
                 <|> try extref
                 <|> try innerref
                 <|> try userproof
                 <|> try groupproof
                 <|> try proofop
                 <|> try proof
                 <|> try plain

innerref :: Parser Expr
innerref = string ">>" >> ((\postId -> InnerRef $ read postId) <$> many1 digit)

extref :: Parser Expr
extref = do
  void $ string ">>"
  void $ optional $ char '/'
  board <- many1 $ noneOf "/\n\t\r "
  void $ char '/'
  post <- many1 digit  
  return $ ExternalRef (escapeHTML $ pack board) $ read post

proof :: Parser Expr
proof = string "##" >> ((\postId -> ProofLabel $ read postId) <$> many1 digit)

proofop :: Parser Expr
proofop = string "##OP" >> return ProofLabelOP

userproof :: Parser Expr
userproof = string "#user" >> return UserProof

groupproof :: Parser Expr
groupproof = string "#group" >> return GroupProof

newline :: Parser Expr
newline = newline' >> return Newline
--------------------------------------------------------------
expr :: Parsec Text () Expr
expr = try quote
   <|> tryTags
   <|> tryWakabaTags
   <|> try link
   <|> try extref
   <|> try innerref
   <|> try userproof
   <|> try groupproof
   <|> try proofop
   <|> try proof
   <|> try newline
   <|> try plain

parseMarkup :: Text -> Either ParseError [Expr]
parseMarkup = parse (many expr) "yoba markup"
