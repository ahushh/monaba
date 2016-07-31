######################################################################################
#### Post form
######################################################################################
## fields
Name: Name
PostSubject: Post's subject
ThreadSubject: Thread's subject
SubmitBtn: Submit
Message: Your message
Password: Password
File: File
Goback: Go back to
ToThread: thread
ToBoard: board
ToFeed: /feed/
Nobump: No bump
Captcha: Captcha

### Post form rules
SupportedFileTypes: Supported file types are
MaxFileSize: Max files size allowed is
ImgsWillBeThumbnailed size@Int: Images greater than #{show size}x#{show size} will be thumbnailed.
OpModerationIsEnabled: OP moderation is enabled on this board.
PostFormBumpLimit bl@Int: Bump limit is #{unlimitedBump bl "∞"}
### misc
ThreadHeaderNumber thr@Int: Thread ##{show thr}
ThreadHeaderReplyTo thr2@Int: Reply to #{show thr2} thread
NewThread: New thread
ClosePostForm: Close form
Reply: Reply
ReplyToThread: Reply to thread №
AttachFile: Attach file
ShowRules: Show rules
HideRules: Hide Rules

### Editing
Edit: Edit
PostEdited: Post has been edited successfully
PostNotYours: Wrong password or session. You can't edit this post.
PostEditingIsDisabled: Post editing is disabled on this board
ThreadEditingIsDisabled: OP editing is disabled on this board
LastModified: Last modified
EditingHistory: Editing history
EditingHistoryIsDisabled: Editing history is disabled
GoTo: Go to
EditingHistoryOfPost post@Int: Editing history of post ##{show post}
YouAlreadyEditedPost n@Int: You have already edited this post #{show n} times.

### Markup
PlainText: Plain text
MarkupBold: bold
MarkupItalic: italic
MarkupUnderline: underline
MarkupStrike: strike
MarkupSpoiler: spoiler
MarkupCode: code ▼
MarkupInlineLatex: inline LaTeX
MarkupLatex: LaTeX
MarkupQuote: >quote

Error err@Text: Error: #{err}
MustBeGreaterThan thing@Text n@Int: '#{thing}' must be greater than #{show n}
######################################################################################
#### Page
######################################################################################
CatalogReplies vv@Int: Replies: #{show vv}
Catalog: Catalog
News: News
Feed: Feed
Iwantmore: I wanna moar
## thread
Open: Open
PostsOmitted n@Int: #{show n} #{plural n "post" "posts"} omitted.
ExpandThread: Expand thread.
ThreadIsExpanded: Thread is expanded.
TruncateThread: Truncate.
NoThreads: No threads yet
## pages
Pages: Pages:
Back: Back
Next: Next
Previous: Previous
DeleteBtn: Delete
MarkAllPosts: Mark all posts
OnlyFiles: Only files
## panel
Manage: Manage
Login: Log in
Logout: Log out
Home: Home
Bookmarks: Bookmarks
### admin buttons
BanPoster: Ban this poster
DeleteByIP: Delete all posts by IP
BanPosterAndDelete: Delete all posts by IP and ban poster
StickThread: Sticky thread
UnstickThread: Unsticky thread
LockThread: Lock
UnlockThread: Unlock
Autosage: Autosage
ThreadIsSticked: Thread is sticked
ThreadIsLocked: Thread is locked
ThreadAutosage: Thread is on autosage
### Deleted posts
DeletedPosts: Deleted posts
DeletedPostsIn thr@Int: Deleted posts in thread ##{show thr}
ShowDeletedPosts: Show deleted posts

######################################################################################
#### Posting messages
######################################################################################
WrongCaptcha: Wrong captcha
NoFileOrText: File or message is required
NoFile: File required
OpFileIsDisabled: OP file attachment is disabled on this board
ThreadTitleIsRequired: Thread subject is required
ReplyFileIsDisabled: Reply file attachment is disabled on this board
UnknownError: Unknown error
BadFormData: Error parsing submitted data
NoFormData: No data was submitted
TooLongMessage maxl@Int: Your message is too long. Max length is #{show maxl}
DeleteNoPosts: Didn't you forget to mark the posts you want to delete?
DeleteWrongPassword: Wrong password
YouAreNotOp: Wrong OP password or session. You can't moderate this thread.
TypeNotAllowed: This file type isn't allowed
NoSuchThread: No such thread
NoSuchBoard: No such board
LockedThread: This thread is locked and can't be posted in
PostSent: Post has been submitted successfully
PostingTooFast: You're posting too fast!

######################################################################################
#### Management
######################################################################################
Management: Management
BanManagement: Ban management
BoardManagement: Board management
Enable: Enabled
Disable: Disabled
Required: Required
XForUnlim x@Text: #{x} for unlimited
Boards: Boards
Bans: Bans
MoveThread: Move thread
WhichBoard: Which board?
ChangeThread: Move post to another thread
WhichThread: Which thread? Input 0 to create a new thread

### Boards
BoardIndexNumber: Index number
BoardName: Board name
BoardTitle: Board title
BoardTitleEG: e.g. random, music
BoardSummary: Board description
BoardSummaryEG: 
BoardBumpLimit: Bump limit
BoardNumberFiles : Max number of files
BoardAllowedTypes: Allowed file types
BoardExtraRules: Extra rules
BoardDefaultName : Default name
BoardMaxMsgLength: Max message length
BoardThumbSize : Thumb max size in pixels
BoardThreadsPerPage: Threads per page        
BoardPreviewsPerThread: Previews per thread 
BoardOpFile: OP file
BoardReplyFile: Reply file
BoardHidden: Hidden
BoardThreadLimit: Max threads
BoardEnableCaptcha: Enable captcha
BoardEnableGeoIp: Show country flags
BoardCategory: Category
BoardCategories: Board categories (separate by comma)
BoardViewAccess: To view this board you must be a
BoardReplyAccess: To reply you must be a
BoardThreadAccess: To create new thread you must be a
BoardEnableForcedAnon: Force anonymity (disable name input)
BoardRequiredThreadTitle: Required thread title
WontBeUpdatedIfNone: These settings won't be updated if their value is <None>
OpModeration: OP moderation
PostEditing: Post editing
OpEditing: OP editing
ShowEditHistory: Show editing history
ShowPostDate: Show post date
BoardEnablePM: Private messages
## board list
NewBoard: New board
AllBoards: All boards
## actions
UpdateBoardsBtn: Update
AddBoardBtn: Add board
DeleteAll: Delete all
CleanBoard: Clean
CleanBoardConfirm: This will delete ALL posts from the board! Are you sure you want to continue?
DeleteBoardConfirm: Are you sure you want to delete the board including ALL posts?
RebuildPostsMessagesOnBoard: Rebuild HTML of posts messages
## info
SeparateBy d@Text: Separate by '#{d}'
SeparateBySpace: Separate by space
Optional: Optional
## Messages
WrongBoardId: Wrong board ID
BoardsUpdated: Boards have been updated successfully
BoardAdded: Board has been added successfully
BoardCleaned: Board has been cleaned successfully
BoardDeleted: Board has been deleted successfully

### Bans management
Ban: Ban
BanIp: IP
BanReason: Reason
BanExpires: Expires
BanExpiresInHours: Expires (in hours)
BanBoard: Board
NeverExpires: NEVER
## Messages
YouAreBanned reason@Text expires@Text: You are banned! Reason: "#{reason}". Your ban expires at #{expires}
BanAdded: Ban has been added successfully
BanDeleted: Ban has been deleted successfully

### Groups management
Group: Group
Groups: Groups
GroupAddOrUpdate: Add/updated
GroupAddedOrUpdated: Group has been added/updated
GroupDoesNotExist: Group doesn't exist
GroupDeleted: Group has been deleted

### Account
Account: Account
OldPassword: Old password
NewPassword: New password
PostsSent: Posts sent
ChangePassword: Change
## Messages
PasswordChanged: Password has been changed successfully
WrongPassword: Wrong password

### Users management
Users: Users
AddNewUserOrUpdate: New user/update
## Messages
UsersAdded: New user has been added successfully
UsersUpdated: User's password/group has been changed successfully
UsersDeleted: User has been deleted successfully
UsersPasswordNotSpecified: Password wasn't specified
UserDoesNotExist: User doesn't exist
UsersDeleteConfirm: This cannot be undone. Continue?
YouAreTheOnlyWhoCanManageUsers: DENIED. You are the only who can manage users.

### Config management
Config: Config
ShowNews: How many news will be displayed
ShowLatestPosts: How many posts will be displayed on /feed page
MaxEditings: How many times you can edit your post
NewsBoard: Board for news
UpdateConfig: Update config
ReplyDelay: Reply delay
ThreadDelay: New thread delay
AdminAboutPage: HTML of about page
AdminHomePage: HTML of home page
ModlogMaxEntriesView: Max modlog entries
ModlogEntriesPerPage: Modlog entries per page
## Messages
ConfigUpdated: Config has been updated successfully

### Permissions
Permissions: Permissions
ManageThread:Manage Thread
ManageBoard :Manage Board
ManageUsers :Manage Users
ManageConfig:Manage Config
DeletePosts :Delete Posts
ManagePanel :Manage Panel
ManageBan   :Manage Ban
AdditionalMarkup: Additional markup
EditPosts: Edit posts and view history
ShadowEdit: Edit posts without saving history
AppControl: Application control
ViewModlog: View moderation log 
ViewIPAndID: View poster's IP and ID
Hellbanning: Hellbanning
ChangeFileRating: Change file's censorship rating

DontSaveHistory: Do not save editing history
######################################################################################
#### Hellbanning
######################################################################################
HellbanUserIsBanned: User is hellbanned
HellbanDo: Hellban
HellbanUndo: Unhellban
HellbanShowPost: Show post
HellbanHidePost: Hide post
HellbanShowAllPosts: Show all posts
HellbanHideAllPosts: Hide all posts
HellbanHide: Hide this post for any user except its author
HellbanPoster: Hellban this user
HideAndHellban: Hide this post and hellban user
HideAllAndHellban: Hide all user's posts and hellban him/her

PostIsVisibleOnlyForAuthor: This post is visible only for its author

HellbanAllPosts: All hidden posts
ShkrinkHellbanned: Shrink hellbanned
######################################################################################
#### Wordfilter
######################################################################################
WordfilterAddBtn: Add
LeaveEmptyToSelectAllBoards: leave empty to select all boards
Wordfilter: Wordfilter
WordfilterBoard: board
WordfilterWords: words separated by space
WordfilterExactMatch: exact match
WordfilterRegex: regex (PCRE)
WordfilterDataType: Type
WordfilterData: What to filter
WordfilterAction: What to do
WordfilterActionMsg: Message displayed to user
WordfilterBan: to ban IP
WordfilterHB: to hellban user
WordfilterHBHide: to hide post
WordfilterDeny: to deny post
WordfilterReplace: to replace
WordfilterDenied: your post is denied
WordfilterReplacedBy: Replaced by
######################################################################################
#### Help
######################################################################################
Help: Help
## Markup
Markup: Markup
IfYouAreOp: If you are OP
IfYouAreNotOp: If you are not OP
IfPostBelongs: If this post belongs to you
IfPostDoesntBelong: If this post doesn't belong to you
ProofLabels: Proof labels
Latex: LaTeX
SourceCode: Source code
SupportedLanguages: Supported languages for tag [code=<language>]

######################################################################################
#### Ajax, popups, refmap, ...
######################################################################################
Online: Online
GetNewPosts: Get new posts
NoNewPosts: No new posts
NewPosts: New posts!
NewPostsLast: Last:

ThreadNotExist: This thread doesn't exist anymore
NoPostsAnymore: Ooops, no posts in this thread!
Loading: Loading...
ConnectionError: Connection error

Replies: Replies:
CloseAllPopupIcon: Close all popups
CloseOnePopupIcon: Close one

MarkToDeleteIcon: Mark to delete
EditIcon: Edit
ReplyIcon: Reply
PrivateMessage: Private message
RemoveFileIcon: Remove file
CloseIcon: Close

PostMessageIsTooLong: Message is too long.
ExpandPost: Show full.
ShrinkPost: Shrink message.

MarkAsRead: Mark as read
######################################################################################
#### Thread hiding
######################################################################################
HideThread: Hide thread
ThreadIsHidden: Thread is hidden.
HiddenThreads: Hidden threads
NoHiddenThreads: No hidden threads
ShowThread: Show thread

######################################################################################
#### Settings
######################################################################################
Settings: Settings
Apply: Apply
Applied: Settings applied
TimeZone: Time zone
Style: Style
Language: Language
FeedIgnoredBoards: /feed/ ignored boards
MaxAllowedRating: Max allowed rating
ResetDefaults: Reset defaults
JSFeatures: JavaScript features

######################################################################################
#### Modlogs
######################################################################################
Modlog: Modlog
ModlogDate: Date
ModlogGroup: Group
ModlogUser: User
ModlogMessage: Message

ModlogBanAdded ip@Text reason@Text nn@Int: Banned IP #{ip} — ##{show nn} — #{reason}
ModlogDelBan nnn@Int: Deleted ban ##{show nnn}

ModlogNewBoard bn@Text: Added a new board /#{bn}/
ModlogUpdateAllBoards: Updated all boards
ModlogUpdateBoard bnn@Text: Updated board /#{bnn}/
ModlogCleanAllBoards: Cleaned all boards
ModlogCleanBoard bnnn@Text: Cleaned board /#{bnnn}/
ModlogDeleteAllBoards: Deleted all boards
ModlogDeleteBoard adsf@Text: Deleted board /#{adsf}/

ModlogUpdateGroup ggp@Text: Updated group #{ggp}
ModlogAddGroup gag@Text: Added group #{gag}
ModlogDelGroup gsd@Text: Deleted group #{gsd}

ModlogAddUser gag@Text: Added user #{gag}
ModlogDelUser gag@Text: Deleted user #{gag}
ModlogChangeUserPassword g@Text: Change password of user #{g}
ModlogChangeUserGroup u@Text g@Text: Change group of user #{u} to #{g}

ModlogUpdateConfig: Updated config

ModlogStickThread thr1@Text: Sticked thread #{thr1}
ModlogLockThread thr1@Text: Locked thread #{thr1}
ModlogAutosageOnThread thr1@Text: Turned off bumping thread #{thr1}

ModlogUnstickThread thr1@Text: Unsticked thread #{thr1}
ModlogUnlockThread thr1@Text: Unlocked thread #{thr1}
ModlogAutosageOffThread thr1@Text: Turned on bumping thread #{thr1}

ModlogHellban uid@Text: Hellbanned user #{uid}
ModlogUnhellban uid@Text: Unhellbanned user #{uid}

ModlogHellbanHidePost p@Text: Hellban: hide post #{p}
ModlogHellbanShowPost p@Text: Hellban: show post #{p}

ModlogHellbanHideAllPosts uid@Text: Hellban: hide all users's posts #{uid}
ModlogHellbanShowAllPosts uid@Text: Hellban: show all users's posts #{uid}

ModlogRebuildPostsMessagesOn board@Text: Rebuilt HTML of posts messages on /#{board}/
ModlogRebuildPostsMessages: Rebuilt HTML of posts messages

ModlogMoveThread from@Text to@Text: Moved thread from #{from} to #{to}
ModlogChangeThread from@Text to@Text p@Text: Moved #{p} from thread #{from} to #{to}

NoSuchPost: No such post

ModlogChangeRating f@Text p@Text r1@Text r2@Text: Censorship rating #{f} #{r1} → #{r2} #{p}

ModlogDisablePostEditing p@Text: Disabled post editing #{p}
ModlogEnablePostEditing p@Text: Enabled post editing #{p}

ModlogRecoverPosts p@Text: Recovered deleted posts #{p}

ModlogDeletePosts p@Text: Deleted posts #{p}
ModlogDeleteFile f@Text p@Text: Deleted file #{f} of post #{p}

ModlogLogin: Log in
ModlogLogout: Log out

ModlogAppRestart: Restarted the application
ModlogExecuteGitPull: Execute 'git pull'

######################################################################################
#### Admin search posts
######################################################################################
Search: Search
AdminSearchUID: Search posts with this user ID
AdminSearchIP: Search posts with this IP
PostsByUserID: Posts by user ID
PostsByIP: Posts by IP
DisplayOnlyHellbanned: Display only hellbanned posts
DisplayAllPosts: Display all posts
AllHellbannedUsers: All hellbaned users
######################################################################################
#### Admin deleted posts
######################################################################################
AdminDeletedPosts: Deleted posts
AdminAllDeletedPosts: All deleted posts
AdminRecoverDeletedPosts: Recover marked posts
AdminRecoveredDeletedPosts: Deleted posts have been recoverd
AdminDeletedFromBoard: from board 
AdminDeletedFromThread: from thread 
AdminDeletedFilter: Filter posts
AdminDeletedWhichBoard: from which board?
AdminDeletedWhichThread: from which thread? 0 — display only OPs, -1 — replies to all threads
######################################################################################
ImageSource: Source
PostPreview: Preview
DeleteFile: Delete file
DeleteFileConfirm: This action cannot be undone. Continue?
FileNotYours: Wrong session. You can't delete this file.
DisabledEditing: Editing of this post is disabled
LockEditing: Disable editing of this post for users
UnlockEditing: Enable editing of this post for users
HomeBoards: Board list
######################################################################################
RssFeed s@Text b@Text: #{s} /#{b}/ RSS feed
RssRecentPosts b@Text: Recents posts from /#{b}/
SuccessEx: Success!
ErrorEx: Error!
AppRestart: Restart Monaba
AppGitPull: Execute 'git pull'
MoveElement: Move
######################################################################################
## JS Settings
######################################################################################
DesktopNotification: desktop_notification
BoardStatsUpdate: board_stats_update
Eventsource: eventsource
ExpandFilesInPost: expand_files_in_post
PostsAutoloadInterval: posts_autoload_interval
Mathjax: mathjax
AnimationSpeed: animation_speed
PopupMessageDisplayTime: popup_message_display_time
PopupPostDelayBeforeDisplaying: popup_post_delay_before_displaying
PopupPostRemovingOnMouseOut: popup_post_removing_on_mouse_out
PpopupPostDelayBeforeRemoving: popup_post_delay_before_removing
VideoRememberVolume: video_remember_volume
VideoDefaultVolume: video_default_volume
VideoAutoplay: video_autoplay
VideoLoop: video_loop
VideoScaleWidth: video_scale_width
VideoScaleHeight: video_scale_height
VimeoEnable: vimeo_enable
CoubEnable: coub_enable
YoutubeEnable: youtube_enable
YoutubeLoadTitles: youtube_load_titles
YoutubeHtml5: youtube_html5
UrlVideoWidth: url_video_width
UrlVideoHeight: url_video_height
FlashWidth: flash_width
FlashHeight: flash_height
IframelyAPIKey: iframely_API_key
YoutubeAPIKey: youtube_API_key
CssWidePosts: css_wide_posts
CssSideMargin: css_side_margin
CssHideFilesNames: css_hide_files_names
CssHideFilesInfo: css_hide_files_info
CssHideMarkupButtons: css_hide_markup_buttons
CssAttachmentThumbWidth: css_attachment_thumb_width
CssCustom: css_custom
######################################################################################
## Statistics
######################################################################################
RecentImages: Recent images
Statistics: Statistics
StatsAllPosts: All posts
StatsAllPostsDeleted del@Int: (not including #{show del} deleted)
StatsMonth: Last month's posts
StatsDay: Last day's posts
StatsAllFiles: All files
######################################################################################
## Bookmarks
######################################################################################
BookmarksAdd: Add to bookmarks
BookmarksDel: Remove from bookmarks
BookmarksNewPosts: New posts
######################################################################################
## Search
######################################################################################
SearchFound nnn@Int: Found #{show nnn} posts
SearchDoc: Full sphinx syntax
SearchHelpOr: operator OR
SearchHelpMaybe: operator MAYBE
SearchHelpNot: operator NOT
SearchHelpPhrase: phrase search operator
