{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Posting where

import           Import
import           Data.Digest.OpenSSL.MD5 (md5sum)
import           Data.Conduit            (($$))
import qualified Data.ByteString         as BS
import qualified Data.Conduit.List       as CL
-------------------------------------------------------------------------------------------------------------------
data GoBackTo = ToThread | ToBoard
    deriving (Show, Read, Eq, Enum, Bounded)
-------------------------------------------------------------------------------------------------------------------
postForm :: Int -> Html -> MForm Handler (FormResult ( Maybe Text     -- name
                                                  , Maybe Text     -- subject
                                                  , Maybe Textarea -- message
                                                  , Text           -- password
                                                  , Maybe Text     -- captcha value
                                                  , [FormResult (Maybe FileInfo)] -- files
                                                  , GoBackTo       -- go back to
                                                  , Maybe Bool)    -- sage (no bump)
                                       , Board        -> -- boardW
                                         Bool         -> -- isthreadW
                                         Maybe Text   -> -- maybeCaptchaInfoW
                                         Maybe Text   -> -- acaptchaW
                                         Bool         -> -- enableCaptchaW
                                         Maybe (Entity Person) -> -- muserW
                                         Widget)
postForm numberFiles extra = do
  lastName   <- lookupSession "name"
  lastGoback <- lookupSession "goback"
  (nameRes     , nameView    ) <- mopt textField              "" (maybe Nothing (Just . Just) lastName)
  (subjectRes  , subjectView ) <- mopt textField              "" Nothing
  (messageRes  , messageView ) <- mopt textareaField          "" Nothing
  (passwordRes , passwordView) <- mreq passwordField          "" Nothing
  (captchaRes  , captchaView ) <- mopt textField              "" Nothing
  (gobackRes   , gobackView  ) <- mreq (selectFieldList urls) "" (Just $ maybe ToBoard (\x -> read $ unpack x :: GoBackTo) lastGoback)
  (nobumpRes   , nobumpView  ) <- mopt checkBoxField          "" Nothing
  (fileresults , fileviews   ) <- unzip <$> forM ([1..numberFiles] :: [Int]) (\_ -> mopt fileField "File" Nothing)
  let result = (,,,,,,,) <$> nameRes <*> subjectRes <*> messageRes <*> passwordRes <*> captchaRes <*>
               FormSuccess fileresults <*> gobackRes <*> nobumpRes
      widget boardW isthreadW maybeCaptchaInfoW acaptchaW enableCaptchaW muserW = $(widgetFile "postform")
  return (result, widget)
    where urls :: [(Text, GoBackTo)]
          urls = [("thread",ToThread), ("board",ToBoard)]
-------------------------------------------------------------------------------------------------------------------
insertFiles :: [FormResult (Maybe FileInfo)] -> Int -> Key Post -> HandlerT App IO ()
insertFiles []    _           _      = return ()
insertFiles files thumbSize postId = forM_ files (\formfile ->
  case formfile of
    FormSuccess (Just f) -> do
      md5                              <- md5sum <$> BS.concat <$> (fileSource f $$ CL.consume) 
      (origfilename, uploadedfilename) <- liftIO $ writeToServer  f md5
      filepath                         <- return $ imageFilePath  filetype uploadedfilename
      filesize                         <- liftIO $ getFileSize    filepath
      filedesc                         <- return $ formatFileSize filesize
      newFile                          <- return  Attachedfile { attachedfileParentId    = postId
                                                              , attachedfileMd5         = pack md5
                                                              , attachedfileName        = uploadedfilename
                                                              , attachedfileOrigName    = origfilename
                                                              , attachedfileType        = filetype
                                                              , attachedfileThumbSize   = thumbSize
                                                              , attachedfileDescription = pack filedesc
                                                              }
      if isImageFile filetype
        then do
          imageresolution <- liftIO $ getImageResolution filepath filetype
          void $ liftIO $ makeThumbImg thumbSize filepath uploadedfilename filetype imageresolution
          void $ runDB $ insert $ newFile { attachedfileDescription = pack $ filedesc ++ ", " ++
                                                                      show (fst imageresolution) ++ "x" ++ show (snd imageresolution) }
        else do
          liftIO $ makeThumbNonImg uploadedfilename filetype
          void $ runDB $ insert newFile
        where filetype = typeOfFile f
    _                    -> return ())
-------------------------------------------------------------------------------------------------------------------
bumpThread :: Text -> Int -> UTCTime -> HandlerT App IO ()
bumpThread board thread now = do
  maybeThread <- runDB $ selectFirst [PostBoard ==. board, PostLocalId ==. thread] []
  case maybeThread of
    Just (Entity thrId _) -> runDB $ update thrId [PostBumped =. Just now]
    _                     -> error "pattern matching failed at bumpThread"
-------------------------------------------------------------------------------------------------------------------
isBanExpired :: Entity Ban -> Handler Bool
isBanExpired (Entity banId ban) = do
  let expires = banExpires ban
  case expires of
    Nothing   -> return False
    Just t    -> do
      now <- liftIO getCurrentTime
      if (floor $ utctDayTime now :: Int) > (floor $ utctDayTime t :: Int)
        then runDB (delete banId) >> return True
        else return False
