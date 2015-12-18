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
PostEdited: Post successfully edited
PostNotYours: Wrong password or session. You can't edit this post.
PostEditingIsDisabled: Post editing is disabled on this board
ThreadEditingIsDisabled: OP editing is disabled on this board
LastModified: Last modified
EditingHistory: Editing history
EditingHistoryIsDisabled: Editing history is disabled
GoTo: Go to
EditingHistoryOfPost post@Int: Editing history of post ##{show post}
YouAlreadyEditedPost n@Int: You've already edited this post #{show n} times.

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
OnlyFiles: Only files
## panel
Manage: Manage
Login: Login
Logout: Logout
Home: Home
### admin buttons
BanPoster: Ban poster
StickThread: Stick/unstick
LockThread: Lock/unlock
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
PostSent: Post successfully sent
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
WhichThread: Which thread? Input 0 to create new thread

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
## board list
NewBoard: New board
AllBoards: All boards
## actions
UpdateBoardsBtn: Update
AddBoardBtn: Add board
DeleteAll: Delete all
CleanBoard: Clean
CleanBoardConfirm: This will delete ALL posts from the board! Are you sure you want to continue?
DeleteBoardConfirm: Are you sure you want to delete the board?
RebuildPostsMessagesOnBoard: Rebuild HTML of posts messages
## info
SeparateBy d@Text: Separate by '#{d}'
SeparateBySpace: Separate by space
Optional: Optional
## Messages
WrongBoardId: Wrong board ID
BoardsUpdated: Boards successfully updated
BoardAdded: Board successfully added
BoardCleaned: Board successfully cleaned
BoardDeleted: Board successfully deleted

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
BanAdded: Ban successfully added
BanDeleted: Ban successfully deleted

### Groups management
Group: Group
Groups: Groups
GroupAddOrUpdate: Add/updated
GroupAddedOrUpdated: Group added/updated
GroupDoesNotExist: Group doesn't exist
GroupDeleted: Group deleted

### Account
Account: Account
NewPassword: New password
## Messages
PasswordChanged: Password successfully changed

### Users management
Users: Users
AddNewUserOrUpdate: New user/update
## Messages
UsersAddedOrUpdated: New user successfully added/updated
UsersDeleted: User successfully deleted
UserDoesNotExist: User doesn't exist
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
AdminAboutPage: Html of about page
AdminHomePage: Html of home page
ModlogMaxEntriesView: Max modlog entries
ModlogEntriesPerPage: Modlog entries per page
## Messages
ConfigUpdated: Config successfully updated

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
ViewModlog: View moderation log 
ViewIPAndID: View poster's IP and ID
Hellbanning: Hellbanning

######################################################################################
#### Hellbanning
######################################################################################
HellbanHide: Hide this post for any user except its author
HellbanPoster: Hellban this user
HideAndHellban: Hide this post and hellban user
HideAllAndHellban: Hide all user's posts and hellban him/her

HellbanShowPost: Show this post for any user
HellbanUnbanUser: Unhellban this user
HellbanShowAndUnban: Show this post and unhellban user

PostIsVisibleOnlyForAuthor: This post is visible only for its author

HellbanAllPosts: All hidden posts

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
Online: Online:
GetNewPosts: Get new posts
NoNewPosts: No new posts

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
ResetDefaults: Reset defaults

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

ModlogUpdateConfig: Updated config

ModlogStickThread thr1@Text: Sticked thread #{thr1}
ModlogLockThread thr1@Text: Locked thread #{thr1}
ModlogAutosageThread thr1@Text: Turned off bumping thread #{thr1}

ModlogHellban uid@Text: Hellbanned user #{uid}
ModlogUnhellban uid@Text: Unhellbanned user #{uid}

ModlogRebuildPostsMessagesOn board@Text: Rebuilt HTML of posts messages on /#{board}/
ModlogRebuildPostsMessages: Rebuilt HTML of posts messages

ModlogMoveThread from@Text to@Text: Move thread from #{from} to #{to}
ModlogChangeThread from@Text to@Text p@Text: Move #{p} from thread #{from} to #{to}

NoSuchPost: No such post

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
######################################################################################
ImageSource: Source
PostPreview: Preview