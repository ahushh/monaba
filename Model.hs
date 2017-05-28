{-# LANGUAGE FlexibleInstances #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import ModelTypes
import Yesod.Auth.HashDB (HashDBUser(..))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h p = p { userPassword = Just h }

instance ToJSON Post where
    toJSON Post {..} = object
        [ "board"       .= postBoard
        , "id"    .= postLocalId
        , "parent"      .= postParent
        , "date"        .= postDate
        , "bumped"      .= postBumped
        , "sticked"     .= postSticked
        , "locked"      .= postLocked
        , "autosage"    .= postAutosage
        , "message"     .= postMessage
        , "rawMessage"  .= postRawMessage
        , "title"       .= postTitle
        , "name"        .= postName
        , "deletedByOp" .= postDeletedByOp
        ]
    
instance ToJSON Attachedfile where
    toJSON Attachedfile {..} = object
        [ "hashsum"         .= attachedfileHashsum
        , "name"        .= attachedfileName
        , "extension"   .= attachedfileExtension
        , "thumbSize"   .= attachedfileThumbSize
        , "thumbWidth"  .= attachedfileThumbWidth
        , "thumbHeight" .= attachedfileThumbHeight
        , "size"        .= attachedfileSize
        , "info"        .= attachedfileInfo
        , "path"        .= attachedfilePath
        , "rating"      .= attachedfileRating  
        -- , "thumb_path"  .= thumbUrlPath appUploadDir appStaticDir attachedfileThumbSiz attachedfileFiletype attachedfileExtension attachedfileHashsum attachedfileOnion 
        ]

instance ToJSON (Entity Attachedfile) where
    toJSON (Entity k v) = object
           [ "id"    .= k
           , "value" .= v
           ]

instance ToJSON (Entity Post) where
    toJSON (Entity k v) = object
           [ "id"    .= k
           , "value" .= v
           ]
