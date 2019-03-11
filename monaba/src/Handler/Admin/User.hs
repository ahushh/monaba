module Handler.Admin.User where

import           Import
import           Yesod.Auth.HashDB (setPassword, validatePass)
import           Handler.Admin.Modlog (addModlogEntry) 
import qualified Data.Text as T (intercalate)
-------------------------------------------------------------------------------------------------------------
-- Users
-------------------------------------------------------------------------------------------------------------
usersForm :: [(Text,Text)] -> -- ^ [(group name, group name)]
            Html          -> -- ^ Extra token
            MForm Handler (FormResult ( Text -- ^ User name
                                      , Maybe Text -- ^ User password
                                      , Text -- ^ User group
                                      ), Widget)
usersForm groups extra = do
  (userNameRes     , userNameView     ) <- mreq textField                "" Nothing
  (userPasswordRes , userPasswordView ) <- mopt passwordField            "" Nothing
  (userGroupRes    , userGroupView    ) <- mreq (selectFieldList groups) "" Nothing
  let result = (,,) <$> userNameRes <*> userPasswordRes <*> userGroupRes
      widget = $(widgetFile "admin/users-form")
  return (result, widget)

getUsersR :: Handler Html
getUsersR = do
  groups <- map ((\x -> (x,x)) . groupName . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  (formWidget, _) <- generateFormPost $ usersForm groups
  users           <- runDB $ selectList ([]::[Filter User ]) []
  defaultLayout $ do
    defaultTitleMsg MsgUsers
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
    FormSuccess (name, mPassword, group) -> do
      mUser <- runDB $ getBy $ UserUniqName name
      let newUser = User { userName     = name
                         , userPassword = Nothing
                         , userGroup    = group
                         }
      case mUser of
        Just (Entity key user) -> do -- update user
          case mPassword of
            Just password -> do
              userWithPassword <- liftIO $ setPassword password newUser
              void $ runDB $ replace key userWithPassword
              addModlogEntry $ MsgModlogChangeUserPassword name
              when (group /= (userGroup user)) $
                addModlogEntry $ MsgModlogChangeUserGroup name group
            Nothing -> do
              when (group /= (userGroup user)) $
                addModlogEntry $ MsgModlogChangeUserGroup name group
              void $ runDB $ replace key newUser
          msgRedirect MsgUsersUpdated
        Nothing -> do -- new user
          case mPassword of
            Just password -> do
              userWithPassword <- liftIO $ setPassword password newUser
              addModlogEntry $ MsgModlogAddUser name
              void $ runDB $ insert userWithPassword
              msgRedirect MsgUsersAdded
            Nothing -> msgRedirect MsgUsersPasswordNotSpecified


getUsersDeleteR :: Text -> Handler Html
getUsersDeleteR usrName = do
  delUsr <- runDB $ selectFirst [UserName ==. usrName] []
  when (isNothing delUsr) $ msgRedirect MsgUserDoesNotExist

  usrGroup <- runDB $ selectFirst [GroupName ==. userGroup (entityVal $ fromJust delUsr)] []
  when (isNothing usrGroup) $ msgRedirect MsgGroupDoesNotExist
 
  users <- runDB $ selectList ([]::[Filter User ]) []
  groups <- runDB $ selectList ([]::[Filter Group]) []

  let gs = map groupName $ filter ((ManageUsersP `elem`) . groupPermissions) $ map entityVal groups
  when ((ManageUsersP `notElem` groupPermissions (entityVal $ fromJust usrGroup) ) || ((>1) $ length $ filter (`elem` gs) $ map (userGroup . entityVal) users)) $ do
    addModlogEntry $ MsgModlogDelUser usrName
    runDB $ deleteWhere [UserName ==. usrName]
    msgRedirect MsgUsersDeleted
  msgRedirect MsgYouAreTheOnlyWhoCanManageUsers
  where msgRedirect msg = setMessageI msg >> redirect UsersR
-------------------------------------------------------------------------------------------------------------
-- Account  
-------------------------------------------------------------------------------------------------------------
newPasswordForm :: Html -> MForm Handler (FormResult (Text, Text), Widget)
newPasswordForm extra = do
  (oldPasswordRes , oldPasswordView ) <- mreq passwordField "" Nothing
  (newPasswordRes , newPasswordView ) <- mreq passwordField "" Nothing
  let widget = toWidget [whamlet|
                             <form method=post action=@{NewPasswordR}>
                                 #{extra}
                                 _{MsgOldPassword}: ^{fvInput oldPasswordView}
                                 _{MsgNewPassword}: ^{fvInput newPasswordView}
                                  <input type=submit value=_{MsgNewPassword}>
                        |]
      result = (,) <$> oldPasswordRes <*> newPasswordRes
  return (result, widget)

getAccountR :: Handler Html
getAccountR = do
  (formWidget, _) <- generateFormPost newPasswordForm
  user  <- (entityVal . fromJust) <$> maybeAuth
  group <- (entityVal . fromJust) <$> runDB (getBy $ GroupUniqName $ userGroup user)
  posts <- runDB $ count [PostOwnerUser ==. Just (userName user)]
  defaultLayout $ do
    defaultTitleMsg MsgAccount
    $(widgetFile "admin/account")
                 
postNewPasswordR :: Handler Html
postNewPasswordR = do
  ((result, _), _) <- runFormPost newPasswordForm
  let msgRedirect msg = setMessageI msg >> redirect AccountR
  case result of
    FormFailure []          -> msgRedirect MsgBadFormData
    FormFailure xs          -> msgRedirect (MsgError $ T.intercalate "; " xs) 
    FormMissing             -> msgRedirect MsgNoFormData
    FormSuccess (oldPassword, newPassword) -> do
      eUser <- fromJust <$> maybeAuth
      let isValid = validatePass (entityVal eUser) oldPassword
      case isValid of
        Just False -> msgRedirect MsgWrongPassword
        _ -> do
           userWithNewPassword <- liftIO $ setPassword newPassword (entityVal eUser)
           void $ runDB $ replace (entityKey eUser) userWithNewPassword
           msgRedirect MsgPasswordChanged
      
