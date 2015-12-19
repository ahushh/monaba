{-# OPTIONS_GHC -O0 #-}
module Handler.Admin.Group where

import           Import
import           Handler.Admin.Modlog (addModlogEntry) 
import qualified Data.Text as T (intercalate)
-------------------------------------------------------------------------------------------------------------
groupsForm :: Html -> MForm Handler (FormResult GroupConfigurationForm, Widget)
groupsForm extra = do
  (nameRes         , nameView        ) <- mreq textField     "" Nothing
  (manageThreadRes , manageThreadView) <- mreq checkBoxField "" Nothing
  (manageBoardRes  , manageBoardView ) <- mreq checkBoxField "" Nothing
  (manageUsersRes  , manageUsersView ) <- mreq checkBoxField "" Nothing
  (manageConfigRes , manageConfigView) <- mreq checkBoxField "" Nothing
  (deletePostsRes  , deletePostsView ) <- mreq checkBoxField "" Nothing    
  (managePanelRes  , managePanelView ) <- mreq checkBoxField "" Nothing
  (manageBanRes    , manageBanView   ) <- mreq checkBoxField "" Nothing
  (editPostsRes    , editPostsView   ) <- mreq checkBoxField "" Nothing
  (aMarkupRes      , aMarkupView     ) <- mreq checkBoxField "" Nothing    
  (viewModlogRes   , viewModlogView  ) <- mreq checkBoxField "" Nothing
  (viewIPAndIDRes  , viewIPAndIDView ) <- mreq checkBoxField "" Nothing
  (hellbanningRes  , hellbanningView ) <- mreq checkBoxField "" Nothing 
  (ratingRes       , ratingView      ) <- mreq checkBoxField "" Nothing

  let result = GroupConfigurationForm <$> nameRes <*>
               manageThreadRes <*> manageBoardRes <*> manageUsersRes <*>
               manageConfigRes <*> deletePostsRes <*> managePanelRes <*>
               manageBanRes    <*> editPostsRes   <*> aMarkupRes     <*>
               viewModlogRes   <*> viewIPAndIDRes <*> hellbanningRes <*>
               ratingRes
      widget = $(widgetFile "admin/groups-form")
  return (result, widget)

showPermission :: Permission -> AppMessage
showPermission p = fromJust $ lookup p xs
  where xs = [(ManageThreadP    , MsgManageThread    )
             ,(ManageBoardP     , MsgManageBoard     )
             ,(ManageUsersP     , MsgManageUsers     )
             ,(ManageConfigP    , MsgManageConfig    )
             ,(DeletePostsP     , MsgDeletePosts     )
             ,(ManagePanelP     , MsgManagePanel     )
             ,(ManageBanP       , MsgManageBan       )
             ,(EditPostsP       , MsgEditPosts       )
             ,(AdditionalMarkupP, MsgAdditionalMarkup)
             ,(ViewModlogP      , MsgViewModlog      )
             ,(ViewIPAndIDP     , MsgViewIPAndID     )
             ,(HellBanP         , MsgHellbanning     )
             ,(ChangeFileRatingP, MsgChangeFileRating)
             ]

getManageGroupsR :: Handler Html
getManageGroupsR = do
  groups <- map entityVal <$> runDB (selectList ([]::[Filter Group]) [])
  (formWidget, _) <- generateFormPost groupsForm
  defaultLayout $ do
    defaultTitleMsg MsgGroups
    $(widgetFile "admin/groups")
  
postManageGroupsR :: Handler Html
postManageGroupsR = do
  ((result, _), _) <- runFormPost groupsForm 
  let msgRedirect msg = setMessageI msg >> redirect ManageGroupsR
  case result of
    FormFailure [] -> msgRedirect MsgBadFormData
    FormFailure xs -> msgRedirect (MsgError $ T.intercalate "; " xs) 
    FormMissing    -> msgRedirect MsgNoFormData
    FormSuccess (GroupConfigurationForm name manageThread manageBoard manageUsers
                 manageConfig  deletePostsP managePanel manageBan editPosts
                 aMarkup viewModLog  viewIPAndID hellbanning changeFileRating
                ) -> do
      let permissions = [(ManageThreadP,manageThread), (ManageBoardP,manageBoard ), (ManageUsersP,manageUsers)
                        ,(ManageConfigP,manageConfig), (DeletePostsP,deletePostsP), (ManagePanelP,managePanel)
                        ,(ManageBanP   ,manageBan   ), (EditPostsP  ,editPosts   ), (AdditionalMarkupP,aMarkup)
                        ,(ViewModlogP  ,viewModLog  ), (ViewIPAndIDP,viewIPAndID ), (HellBanP,hellbanning)
                        ,(ChangeFileRatingP, changeFileRating)
                        ]
          newGroup = Group { groupName        = name
                           , groupPermissions = map fst $ filter snd permissions
                           }
      g <- runDB $ getBy $ GroupUniqName name
      if isJust g
        then (addModlogEntry $ MsgModlogUpdateGroup name) >> (void $ runDB $ replace (entityKey $ fromJust g) newGroup)
        else (addModlogEntry $ MsgModlogAddGroup    name) >> (void $ runDB $ insert newGroup)
      msgRedirect MsgGroupAddedOrUpdated

getDeleteGroupsR :: Text -> Handler ()
getDeleteGroupsR group = do
  delGroup <- runDB $ selectFirst [GroupName ==. group] []
  when (isNothing delGroup) $ setMessageI MsgGroupDoesNotExist >> redirect ManageGroupsR
  usrGroup <- getMaybeGroup =<< maybeAuth
  when (isNothing usrGroup) $ notFound

  groups <- map (groupPermissions . entityVal) <$> runDB (selectList ([]::[Filter Group]) [])
  when ((ManageUsersP `notElem` groupPermissions (entityVal $ fromJust delGroup) ) || ((>1) $ length $ filter (ManageUsersP `elem`) groups)) $ do
    void $ runDB $ deleteWhere [GroupName ==. group]
    addModlogEntry $ MsgModlogDelGroup group 
    setMessageI MsgGroupDeleted >> redirect ManageGroupsR
  setMessageI MsgYouAreTheOnlyWhoCanManageUsers >> redirect ManageGroupsR
