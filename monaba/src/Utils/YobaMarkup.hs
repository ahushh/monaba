{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Utils.YobaMarkup
       (
         doYobaMarkup
       , fixReferences
       , makeExternalRef
       ) where

import           Import
import           System.Process 
import           System.Random
import           System.Exit (ExitCode(..))
import           Text.HTML.TagSoup  (escapeHTML)
import           Text.Parsec hiding (newline)
import           Text.Parsec.Text
import           Text.Shakespeare.Text
import qualified Data.Text     as T (concat, append, intercalate)
import           Control.Monad (replicateM)
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
          | ExternalProofLabel Text Int -- ##/b/1717
          | ProofLabelOP         -- ##OP
          | Color  Text   [Expr] -- [color=red]blah-blah[/color]
          | Dice  Int Int Int    -- [dice]2d100+5[/dice]
          | GroupProof           -- #group
          | UserProof            -- #user
          | Link          Text   -- http://rossia3.ru
          | NamedLink Text Text  -- [Портал сетевой войны](http://rossia3.ru)
          | Magnet    Text Text  -- magnet:?xl=Размер_в_байтах&dn=Имя_файла&xt=urn:tree:tiger:TTH-хеш_файла
          | List        [[Expr]] -- * one\n * two\n * three\n
          | Plain         Text   -- any text 
          | Newline              -- \n
          deriving (Show)
-------------------------------------------------------------------------------------------------------------------
php :: String
php = "/usr/bin/php"
-------------------------------------------------------------------------------------------------------------------
-- Parse only external referency
-------------------------------------------------------------------------------------------------------------------
makeExternalRef :: Text -> Int -> Handler Text
makeExternalRef board post = let parsed = parse (many $ try extref) "yoba markup" $ T.concat [">>/",board,"/",tshow post]
                             in case parsed of
                               Right xs -> unTextarea <$> processMarkup xs "nope" 0
                               Left err -> return $ pack $ show err

-------------------------------------------------------------------------------------------------------------------
-- Fix post referencies after thread moving
-------------------------------------------------------------------------------------------------------------------
plain' :: Parser Expr
plain' = Plain . pack <$> many1 (myCheck >> anyChar)
  where myCheck = foldr (\f acc -> acc >> notFollowedBy (try f)) (notFollowedBy $ try proof) [innerref]

onlyRefs :: Parsec Text () Expr
onlyRefs = try proof
       <|> try innerref
       <|> try plain'

parseOnlyRefs :: Text -> Either ParseError [Expr]
parseOnlyRefs = parse (many onlyRefs) "yoa markup"

processFixRefs :: [Expr] -> Text -> [(Int,Int)] -> IO Textarea
processFixRefs xs oldBoard ids = Textarea <$> foldM f "" xs
  where
    oldIds = map fst ids
    ----------------------------------------------------------------------------------------------------------
    f acc (Plain         x) = return $ acc <> x -- remain unchanged
    ----------------------------------------------------------------------------------------------------------
    f acc (InnerRef postId) = do
      if postId `elem` oldIds
        then return $ acc <> ">>"  <> (tshow $ fromJust $ lookup postId ids)
        else return $ acc <> ">>/" <> oldBoard <> "/" <> (tshow postId)
    ----------------------------------------------------------------------------------------------------------
    -- Don't think there is a need to implement the following
    ----------------------------------------------------------------------------------------------------------
    -- f acc (ExternalRef board postId) = do
    ----------------------------------------------------------------------------------------------------------
    -- f acc (ExternalProofLabel board' postId) = do
    ----------------------------------------------------------------------------------------------------------
    f acc (ProofLabel postId) = do
      if postId `elem` oldIds
        then return $ acc <> "##"  <> (tshow $ fromJust $ lookup postId ids)
        else return $ acc <> "##/" <> oldBoard <> "/" <> (tshow postId)
    f acc _ = return acc

fixReferences :: Text -> [(Int,Int)] -> Textarea -> IO Textarea
fixReferences oldBoard ids s = do
  let parsed = parseOnlyRefs $ unTextarea s
  case parsed of
    Right xs -> processFixRefs xs oldBoard ids
    Left err -> return $ Textarea $ pack $ show err

-------------------------------------------------------------------------------------------------------------------
-- Yoba markup
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
codeHighlight :: Text -> Text -> Text -> IO Text
codeHighlight lang source geshi = do
  result <- readProcessWithExitCode php [unpack geshi, unpack lang] (unpack source)
  case result of
    (ExitSuccess, x, _) -> return $ pack x
    _                   -> return $ source

processMarkup :: [Expr] -> Text -> Int -> Handler Textarea
processMarkup xs board thread = Textarea <$> foldM f "" xs
  where
    ----------------------------------------------------------------------------------------------------------
    -- Just helpers
    ----------------------------------------------------------------------------------------------------------
    openSpoiler = "<span class='spoiler'>"
    openStrike  = "<span style='text-decoration:line-through'>"
    refHtml :: Text -> Text -> Text -> Text -> Text -> Text -> Text
    refHtml acc brd "0" p ref pId = [st|#{acc}<a data-post-id=#{pId} data-post-local-id=#{p} data-board=#{brd} data-thread-local-id=0 onmouseover="showPopupPost(this,event,#{pId},#{p},'#{brd}')" onclick='highlightPost("p#{pId}")' href='/#{brd}/#{p}'>#{ref}</a> |]
    refHtml acc brd thr p ref pId = [st|#{acc}<a data-post-id=#{pId} data-post-local-id=#{p} data-board=#{brd} data-thread-local-id=#{thr} onmouseover="showPopupPost(this,event,#{pId},#{p},'#{brd}')" onclick='highlightPost("p#{pId}")' href='/#{brd}/#{thr}#p#{pId}'>#{ref}</a> |]
    getUserName  = userName  . entityVal . fromJust
    getGroupName = groupName . entityVal . fromJust
    li         g = "<li>" <> g <> "</li>"
    ----------------------------------------------------------------------------------------------------------
    boldHandler      acc x = (\g -> acc <> "<strong>" <> g <> "</strong>") <$> foldM f "" x
    italicHandler    acc x = (\g -> acc <> "<em>"     <> g <> "</em>"    ) <$> foldM f "" x
    spoilerHandler   acc x = (\g -> acc <> openSpoiler<> g <> "</span>"  ) <$> foldM f "" x
    strikeHandler    acc x = (\g -> acc <> openStrike <> g <> "</span>"  ) <$> foldM f "" x
    underlineHandler acc x = (\g -> acc <> "<u>"      <> g <> "</u>"     ) <$> foldM f "" x
    quoteHandler     acc x = (\g -> acc <> "<span class=quote>>" <> g <>"</span><br>") <$> foldM f "" x
    listHandler      acc x = (\g -> acc <> "<ul>"     <> g <> "</ul>"    ) <$> T.concat <$> (mapM ((li <$>) . foldM f "") x)
    ----------------------------------------------------------------------------------------------------------
    diceHandler acc n m mo = do
      numbers <- liftIO ( map (+mo) <$> (replicateM n $ randomRIO (1,m)) )
      return $ acc <> "<span class='dice-roll'><img width='24' src='/static/img/roll.svg'>"
                   <> (T.intercalate ", " $ map tshow $ numbers)
                   <> (if length numbers > 1 then " = " <> tshow (sum numbers) else "")
                   <> " (" <> tshow n <> "d" <> tshow m <> yoba mo
                   <> ")</span>"
      where yoba x | x == 0     = ""
                   | x > 0     = "+" <> tshow x
                   | otherwise = tshow x
    ----------------------------------------------------------------------------------------------------------
    colorHandler acc color' msg = do
      muser  <- maybeAuth
      mgroup <- getMaybeGroup muser
      if isNothing muser || isNothing mgroup || (AdditionalMarkupP `notElem` getPermissions mgroup)
        then (\g -> [st|#{acc}[color=#{color'}]#{g}[/color]|]) <$> foldM f "" msg
        else (\g -> [st|#{acc}<span style='color:#{color'}'>#{g}</span>|]) <$> foldM f "" msg
    ----------------------------------------------------------------------------------------------------------
    userproofHandler acc = do
      muser  <- maybeAuth
      mgroup <- getMaybeGroup muser
      if isNothing muser || isNothing mgroup || (AdditionalMarkupP `notElem` getPermissions mgroup)
        then return $ acc <> "#user"
        else return $ acc <> "<span style='border-bottom: 1px red dashed'>#" <> getUserName muser <> "</span>"
    ----------------------------------------------------------------------------------------------------------
    groupproofHandler acc = do
      muser  <- maybeAuth
      mgroup <- getMaybeGroup muser
      if isNothing muser || isNothing mgroup || (AdditionalMarkupP `notElem` getPermissions mgroup)
        then return $ acc <> "#group"
        else return $ acc <> "<span style='border-bottom: 1px red dotted'>#" <> getGroupName mgroup <> "</span>"
    ----------------------------------------------------------------------------------------------------------
    -- Function that process each Expr
    ----------------------------------------------------------------------------------------------------------
    f acc (Code  lang code') = do
      AppSettings{..} <- appSettings <$> getYesod
      T.append acc <$> liftIO (codeHighlight lang code' appHighlight)
    f acc (Plain          x) = return $ T.append acc $ escapeHTML x
    f acc (Color color' msg) = colorHandler      acc color' msg
    f acc (Dice  n   m   mo) = diceHandler       acc n m mo
    f acc (Bold           x) = boldHandler       acc x
    f acc (Italic         x) = italicHandler     acc x
    f acc (Underline      x) = underlineHandler  acc x
    f acc (Strike         x) = strikeHandler     acc x
    f acc (Spoiler        x) = spoilerHandler    acc x
    f acc (SpoilerWakaba  x) = spoilerHandler    acc x
    f acc (Quote          x) = quoteHandler      acc x
    f acc GroupProof         = groupproofHandler acc
    f acc UserProof          = userproofHandler  acc
    f acc (List           x) = listHandler       acc x
    f acc (Link           x) = return $ [st|#{acc}<a href='#{x}'>#{x}</a>|]
    f acc (NamedLink    n x) = return $ [st|#{acc}<a href='#{x}'>#{n}</a>|]
    f acc (Magnet       n x) = return $ [st|#{acc}<a href='#{x}'>#{n}</a>|]
    f acc Newline            = return $ acc <> "<br>"
    ----------------------------------------------------------------------------------------------------------
    f acc (InnerRef postId) = do
      maybePost <- runDB $ selectFirst [PostLocalId ==. postId, PostBoard ==. board] []
      let p = pack $ show postId
      case maybePost of
        Just (Entity pKey pVal) -> do
          let parent = pack $ show $ postParent pVal
          return $ refHtml acc board parent p (">>" <> p) (tshow $ fromSqlKey pKey)
        Nothing              -> return $ acc <> ">>" <> p
    ----------------------------------------------------------------------------------------------------------
    f acc (ExternalRef board' postId) = do
      maybePost <- runDB $ selectFirst [PostLocalId ==. postId, PostBoard ==. board'] []
      let p = pack $ show postId
      case maybePost of
        Just (Entity pKey pVal) -> do
          let parent = pack $ show $ postParent pVal
          return $ refHtml acc board' parent p (">>/" <> board' <> "/" <> p) (tshow $ fromSqlKey pKey)
        Nothing              -> return $ acc <> ">>/" <> board' <> "/" <> p
    ----------------------------------------------------------------------------------------------------------
    f acc (ProofLabel    postId) = do
      posterId  <- getPosterId
      maybePost <- runDB $ selectFirst [PostLocalId ==. postId, PostBoard ==. board] []
      let p = pack $ show postId
      case maybePost of
        Just (Entity pKey pVal) -> do
          let posterId' = postPosterId pVal
              parent    = pack $ show (postParent pVal)
              spanClass = if posterId == posterId' then "post-label-true" else "post-label-false"
              link'     = refHtml "" board parent p ("##" <> p) (tshow $ fromSqlKey pKey)
          return $ acc <> "<span class='" <> spanClass <> "'>" <> link' <> "</span>"
        Nothing              -> return $ acc <> "##" <> p
    ----------------------------------------------------------------------------------------------------------
    f acc (ExternalProofLabel board' postId) = do
      posterId  <- getPosterId
      maybePost <- runDB $ selectFirst [PostLocalId ==. postId, PostBoard ==. board'] []
      let p = pack $ show postId
      case maybePost of
        Just (Entity pKey pVal) -> do
          let posterId' = postPosterId pVal
              parent    = pack $ show (postParent pVal)
              spanClass = if posterId == posterId' then "post-label-true" else "post-label-false"
              link'     = refHtml "" board' parent p ("##/" <> board' <> "/" <> p) (tshow $ fromSqlKey pKey)
          return $ acc <> "<span class='" <> spanClass <> "'>" <> link' <> "</span>"
        Nothing              -> return $ acc <> "##" <> board' <> "/" <> p
    ----------------------------------------------------------------------------------------------------------
    f acc ProofLabelOP = do
      posterId  <- getPosterId
      maybePost <- runDB $ selectFirst [PostLocalId ==. thread, PostBoard ==. board] []
      let t = pack $ show thread
      case maybePost of
        Just (Entity pKey pVal) -> do
          let posterId' = postPosterId pVal
              parent    = pack $ show (postParent pVal)
              spanClass = if posterId == posterId' then "post-label-true" else "post-label-false"
              link'     = refHtml "" board parent t "##OP" (tshow $ fromSqlKey pKey)
          return $ acc <> "<span class='" <> spanClass <> "'>" <> link' <> "</span>"
        Nothing              -> return $ acc <> "##OP"
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
        tagEnds   = map string ["[/i]","[/s]","[/u]","[/spoiler]","[/color]","[/dice]","%%"]
        myCheck'  = foldr (\f acc -> acc >> notFollowedBy (try f))
                          (notFollowedBy $ try quote)
                          wholeTags
        wholeTags = [ spoilerwakaba, newline, namedLink, link, magnet, bold, italic, underline, strike, spoiler,
                      color, code, extref, innerref, proof, extproof, proofop, userproof, groupproof, dice]
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

dice :: Parsec Text () Expr
dice = do
  void $ string "[dice]"
  n <- nat
  void $ oneOf "dд"
  m <- nat
  mo <- option 0 (do{ sign <- do{ void (char '-'); return negate } <|> do{ void (char '+'); return id }
                   ; mo'  <- many1 digit
                   ; return $ sign $ read mo'
                   })
  void $ string "[/dice]"
  return $ Dice (read n) (read m) mo

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
      <|> try dice
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
nat :: Stream s m Char => ParsecT s u m [Char]
nat = do
  n0 <- oneOf "123456789"
  n  <- many digit
  return $ n0:n
  
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

namedLink :: Parser Expr
namedLink = do
  void $ char '['
  name <- many1 $ noneOf "]"
  void $ string "]("
  scheme <- urlScheme
  void $ string "://"
  rest   <- many1 $ noneOf " \n\t\r[)"
  void $ char ')'
  return $ NamedLink (pack name) (T.concat [pack scheme, "://", escapeHTML (pack rest)])

link :: Parser Expr
link = do
  scheme <- urlScheme
  void $ string "://"
  rest   <- many1 $ noneOf " \n\t\r["
  return $ Link (T.concat [pack scheme, "://", escapeHTML (pack rest)])

magnet :: Parser Expr
magnet = do
  scheme <- try $ string "magnet:?"
  left   <- manyTill (noneOf " \n\t\r") (try $ string "dn=")
  name <- many1 $ noneOf " \n\t\r&"
  rest <- many1 $ noneOf " \n\t\r"
  return $ Magnet ("magnet: " <> pack name) (escapeHTML $ T.concat $ map pack [scheme, left, "dn=", name, rest])

quote :: Parser Expr
quote = char '>' >> Quote <$> manyTill onelineExpr (newline' <|> (eof >> return ""))

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

extproof :: Parser Expr
extproof = do
  void $ string "##"
  void $ optional $ char '/'
  board <- many1 $ noneOf "/\n\t\r "  
  void $ char '/'
  post <- many1 digit
  return $ ExternalProofLabel (escapeHTML $ pack board) $ read post

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

list :: Parser Expr
list = do
  entries <- many1 entry
  return $ List entries
  where entry = char '*' >> manyTill (onelineExpr <|> quote) (newline' <|> (eof >> return ""))
--------------------------------------------------------------
onelineExpr :: Parsec Text () Expr
onelineExpr = try bold
          <|> try italic
          <|> try underline
          <|> try strike
          <|> try spoiler
          <|> try color
          <|> try dice
          <|> tryWakabaTags
          <|> try namedLink
          <|> try link
          <|> try magnet
          <|> try extref
          <|> try innerref
          <|> try userproof
          <|> try groupproof
          <|> try proofop
          <|> try proof
          <|> try extproof
          <|> try plain

expr :: Parsec Text () Expr
expr = try quote
   <|> tryTags
   <|> tryWakabaTags
   <|> try namedLink
   <|> try link
   <|> try magnet
   <|> try extref
   <|> try innerref
   <|> try userproof
   <|> try groupproof
   <|> try proofop
   <|> try proof
   <|> try extproof
   <|> try list
   <|> try newline
   <|> try plain

parseMarkup :: Text -> Either ParseError [Expr]
parseMarkup = parse (many expr) "yoba markup"
