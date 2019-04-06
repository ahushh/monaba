module Import.File where

import Prelude
import Yesod
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import           System.FilePath         ((</>))
import           Data.Char               (isPrint, toLower)
import ModelTypes

-- | List of files types which have thumbnails
thumbFileTypes :: [FileType]
thumbFileTypes = [FileVideo, FileImage] --, FileSource, FileDoc] TODO: thumbnails for docs and source files

sanitizeFileName :: String -> String
sanitizeFileName = filter (\x -> x `notElem` ("\\/"::String) && isPrint x)

fileExt :: FileInfo -> String
fileExt = map toLower . reverse . takeWhile (/='.') . reverse . sanitizeFileName . unpack . fileName

extractFileExt :: String -> String
extractFileExt = map toLower . reverse . takeWhile (/='.') . reverse
-------------------------------------------------------------------------------------------------------------------
-- Paths
-------------------------------------------------------------------------------------------------------------------
geoIconPath :: String -> Text -> Text
geoIconPath staticDir code = pack $  "/" </> staticDir </> "geoicons" </> (unpack $ (T.toLower code) <> ".png")

captchaFilePath :: String -> String -> String
captchaFilePath staticDir file = staticDir </> "captcha" </> file
-- Thumbnails
choseFileIcon :: FileType -> String
choseFileIcon ft = case ft of
    FileAudio      -> "audio"
    FileFlash      -> "flash"
    FileArchive    -> "archive"
    FileUndetected -> "default"
    _              -> "default"

thumbIconExt :: String
thumbIconExt = "png"

thumbDirectory :: FilePath
thumbDirectory = "thumb"

thumbUrlPath :: String -> String -> Int -> FileType -> String -> String -> Bool -> FilePath
thumbUrlPath uploadDir staticDir size filetype fileext hashsum onion = "/" </>
  (thumbFilePath (if onion then uploadDir </> "onion" else uploadDir) staticDir size filetype fileext hashsum)

thumbFilePath :: String -> String -> Int -> FileType -> String -> String -> FilePath
thumbFilePath uploadDir staticDir size filetype fileext hashsum
  | filetype == FileVideo           = uploadDir </> thumbDirectory </> (show size ++ "thumb-" ++ hashsum ++ ".png")
  | filetype `elem` thumbFileTypes = uploadDir </> thumbDirectory </> (show size ++ "thumb-" ++ hashsum ++ "." ++ fileext)
  | otherwise                      = staticDir </> "fileicons" </> ((choseFileIcon filetype) ++ "." ++ thumbIconExt)


