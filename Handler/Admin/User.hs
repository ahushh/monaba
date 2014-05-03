{-# LANGUAGE TupleSections, OverloadedStrings, ExistentialQuantification #-}
module Handler.Admin.User where

import           Import
import           Yesod.Auth
import           Yesod.Auth.HashDB    (setPassword)
import qualified Data.Text            as T
import           Handler.Admin.Modlog (addModlogEntry)
-------------------------------------------------------------------------------------------------------------
-- Users
-------------------------------------------------------------------------------------------------------------
usersForm :: [(Text,Text)] -> -- ^ [(group name, group name)]
            Html          -> -- ^ Extra token
            MForm Handler (FormResult ( Text -- ^ User name
                                      , Text -- ^ User password
                                      , Text -- ^ User group
                                      ), Widget)
usersForm groups extra = do
  (userNameRes     , userNameView     ) <- mreq textField                "" Nothing
  (userPasswordRes , userPasswordView ) <- mreq textField                "" Nothing
  (userGroupRes    , userGroupView    ) <- mreq (selectFieldList groups) "" Nothing
  let result = (,,) <$> userNameRes <*> userPasswordRes <*> userGroupRes
      widget = $(widgetFile "admin/users-form")
  return (result, widget)

getUsersR :: Handler Html
getUsersR = do
  muser  <- maybeAuth
  permissions <- getPermissions <$> getMaybeGroup muser

  groups <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  (formWidget, formEnctype) <- generateFormPost $ usersForm groups

  users           <- runDB $ selectList ([]::[Filter User ]) []
  nameOfTheBoard  <- extraSiteName <$> getExtra
  msgrender       <- getMessageRender
  defaultLayout $ do
    setTitle $ toHtml $ nameOfTheBoard <> titleDelimiter <> msgrender MsgUsers
    $(widgetFile "admin/users")

postUsersR :: Handler Html
postUsersR = do
  groups <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])  
  ((result, _), _) <- runFormPost $ usersForm groups
  let msgRedirect msg = setMessageI msg >> redirect UsersR
  case result of
    FormFailure []                      -> msgRedirect MsgBadFormData
    FormFailure xs                      -> msgRedirect (MsgError $ T.intercalate "; " xs) 
    FormMissing                         -> msgRedirect MsgNoFormData
    FormSuccess (name, password, group) -> do
      let newUser = User { userName     = name
                         , userPassword = ""
                         , userSalt     = ""
                         , userGroup    = group
                         }
      userWithPassword <- liftIO $ setPassword password newUser
      u <- runDB $ getBy $ UserUniqName name
      if isJust u
        then void $ runDB $ replace (entityKey $ fromJust u) userWithPassword
        else addModlogEntry (MsgModlogAddUser name) >> void (runDB $ insert userWithPassword)
      msgRedirect MsgUsersAddedOrUpdated

getUsersDeleteR :: Text -> Handler Html
getUsersDeleteR usrName = do
  delUsr   <- runDB $ selectFirst [UserName ==. usrName] []
  when (isNothing delUsr) $ msgRedirect MsgUserDoesNotExist

  usrGroup <- runDB $ selectFirst [GroupName ==. userGroup (entityVal $ fromJust delUsr)] []
  when (isNothing usrGroup) $ msgRedirect MsgGroupDoesNotExist

  users  <- runDB $ selectList ([]::[Filter User ]) []
  groups <- runDB $ selectList ([]::[Filter Group]) []
  let gs = map groupName $ filter ((ManageUsersP `elem`) . groupPermissions) $ map entityVal groups
  when ((ManageUsersP `notElem` groupPermissions (entityVal $ fromJust usrGroup) ) ||
       ((>1) $ length $ filter (`elem` gs) $ map (userGroup . entityVal) users)) $ do
    addModlogEntry $ MsgModlogDelUser usrName
    runDB $ deleteWhere [UserName ==. usrName]
    msgRedirect MsgUsersDeleted
  msgRedirect MsgYouAreTheOnlyWhoCanManageUsers
    where msgRedirect msg = setMessageI msg >> redirect UsersR
