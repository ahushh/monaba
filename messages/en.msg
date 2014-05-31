######################################################################################
#### Post form
######################################################################################
## fields
Name: Name
Subject: Subject
SubmitBtn: Submit
Message: Message
Password: Password
File: File
Goback: Go back to
ToThread: thread
ToBoard: board
Nobump: No bump

### Post form rules
ShowRules: Show rules
HideRules: Hide rules
SupportedFileTypes: Supported file types are
MaxFileSize: The maximum size of all attached files is
ImgsWillBeThumbnailed s@Int: Images greater than #{show s}x#{show s} will be thumbnailed.
OpModerationIsEnabled: OP moderation is enabled on this board.
PostFormBumpLimit b@Int: Bump limit is #{unlimitedBump b "∞"}
### misc
ThreadHeaderNumber t@Int: Thread №#{show t}
ThreadHeaderReplyTo t@Int: Reply to #{show t} thread
NewThread: New thread
ClosePostForm: Close post form
Reply: Reply
ReplyToThread: Reply to thread №

### Editing
Edit: Edit
PostEdited: Post successfully edited
PostNotYours: Wrong password or session. You can't modify this post.
PostEditingIsDisabled: Post editing is disabled on this board
ThreadEditingIsDisabled: OP editing is disabled on this board
LastModified: Last modified
EditingHistory: Editing history
EditingHistoryIsDisabled: Editing history is disabled
GoTo: Go to
EditingHistoryOfPost desu@Int: Editing history of post №#{show desu}
YouAlreadyEditedPost boku@Int: You've already edited this post #{show boku} times.
EditPosts: Edit posts and view history
ShadowEdit: Edit posts without saving history
DontSaveHistory: Do not save editing history

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

FileUpload: Upload file
Error lolka@Text: Error: #{lolka}
MustBeGreaterThan qwe@Text qw@Int: '#{qwe}' must be greater than #{show qw}
######################################################################################
#### Page
######################################################################################
News: News
RecentPosts: Recent posts
## thread
OpenThread: Open thread
PostsOmitted n@Int: #{show n} #{plural n "post" "posts"} omitted.
ExpandThread: Expand thread.
ThreadIsExpanded: Thread is expanded.
TruncateThread: Truncate.
NoThreads: No threads yet
## pages
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
DeletedPostsIn z@Int: Deleted posts in thread №#{show z}
ShowDeletedPosts: Show deleted posts

######################################################################################
#### Posting messages
######################################################################################
WrongCaptcha: Wrong captcha
NoFileOrText: File or message is required
NoFile: File required
OpFileIsDisabled: OP file attachment is disabled on this board
ReplyFileIsDisabled: Reply file attachment is disabled on this board
UnknownError: Unknown error
BadFormData: Bad input
NoFormData: No input
TooLongMessage n@Int: Your message is too long. Max length is #{show n}
PostsDeleted: Posts were successfully deleted
DeleteNoPosts: Didn't you forget to mark the posts you want to delete?
DeleteWrongPassword: Wrong password
YouAreNotOp: Wrong OP password or session. You can't moderate this thread.
TypeNotAllowed: This filetype isn't allowed
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

### Boards
BoardName: Name
BoardDescription : Short description
BoardDescriptionEG: e.g. random, music
BoardLongDescription: Long description
BoardLongDescriptionEG: 
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
BoardThreadAccess: To crate new thread you must be a
BoardEnableForcedAnon: Force anonymity (disable name input)
BoardShowPostDate: Show post date
WontBeUpdatedIfNone: These settings won't be updated if their value is <None>
OpModeration: OP moderation
PostEditing: Post editing
OpEditing: OP editing
ShowEditHistory: Show editing history
## board list
NewBoard: New board
AllBoards: All boards
## actions
UpdateBoardsBtn: Update
AddBoardBtn: Add board
DeleteAll: Delete all
CleanBoard: Clean
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
InvalidData: Invalid data
UpdateBoardsInvalidInput: Invalid input

### Bans management
Ban: Ban
BanIp: IP
BanReason: Reason
BanExpires: Expires
BanExpiresInHours: Expires (in hours)
BanBoard: Board
NeverExpires: Never
## Messages
YouAreBanned reason@Text expires@Text: You are banned! Reason: "#{reason}". Your ban expires at #{expires}
BanAdded: Ban successfully added
BanDeleted: Ban successfully deleted

### Groups management
Group: Group
Groups: Groups
GroupAddOrUpdate: Add/updated
GroupAddedOrUpdated: Group added/updated
GroupDoesNotExist: Group dosn't exist
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
YouAreTheOnlyWhoCanManageUsers: You are the only who can manage users!

### Config management
Config: Config
CaptchaLength: Captcha length
ACaptchaGuards: Guards of adaptive captcha
CaptchaTimeout: Captcha timeout
ShowNews: How many news show
ShowRecentPosts: How many recent posts show
MaxEditings: How many times you can edit your post
NewsBoard: Board for news
UpdateConfig: Update config
ReplyDelay: Reply delay
ThreadDelay: New thread delay
DisplaySage: Display sage icon
ModlogMaxEntriesView: Max modlog entries
ModlogEntriesPerPage: Modlog entries per page
## Messages
ConfigUpdated: Config successfully updated

### Permissions
Permissions: Permissions
ViewIPAndID: View poster's IP and ID
ViewModlog: View moderation log
ManageThread:Manage threads
ManageBoard :Manage boards
ManageUsers :Manage users
ManageConfig:Manage configuration
DeletePosts :Delete posts
ManagePanel :View manage panel
ManageBan   :Manage bans
AdditionalMarkup: Additional markup
Hellbanning :Hellbanning
ChangeFileRating: Change file's censorship rating

### search posts
Search: Search
AdminSearchUID: Search posts with this user ID
AdminSearchIP: Search posts with this IP
PostsByUserID: Posts by user ID
PostsByIP: Posts by IP
DisplayOnlyHellbanned: Display only hellbanned posts
DisplayAllPosts: Display all posts

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
About: About
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
### API
Api: API
ApiPostsAfter: All posts after {post id} in the thread
ApiLastPosts: Last {count} posts in the thread
ApiAllPosts: All posts in the thread
ApiDeletedPosts: All posts deleted by OP in the thread
ApiPost: One post
ApiParameters: Parameters
ApiBoardName: Board name (e.g., b, d, rm)
ApiThreadId: Displayed thread ID
ApiPostId: Displayed post ID
ApiCount: Number of posts

######################################################################################
#### Captcha
######################################################################################
Captcha: Captcha
NoCaptchaInDB: There is no captcha in the DB for this user
TypeOnly: Type only 
ItalicChars: italic characters
BoldChars: bolded characters
RegularChars: regular (not bolded and not italic) characters
YouDontNeedCaptcha: You don't have to enter captcha

######################################################################################
#### Ajax, popups, refmap, ...
######################################################################################
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
AddFileIcon: Add one more file
CloseIcon: Close

PostMessageIsTooLong: Message is too long.
ExpandPost: Show full.
ShrinkPost: Shrink message.

Sage: sage

######################################################################################
#### Thread hiding
######################################################################################
HideThread: Hide thread
HideThreadCompletely: Unsee it!
ThreadIsHidden: Thread is hidden.
HiddenThreads: Hidden threads
UnhideThread: Unhide thread
ShowThread: Show thread

######################################################################################
#### Settings
######################################################################################
Settings: Settings
Apply: Apply
Applied: Settings applied
TimeZone: Time zone
Style: Style
MaxAllowedRating: Max allowed rating
Language: Language
UseMathjax: Use MathJax
StickyTopbar: Sticky topbar
PlainTopbar: Plain topbar
PpleerEmbedByClick: Embed pleer.com by click
WidePosts: Wide posts
CenteredPosts: Centered posts
UseEventSource: Post autoload (EventSource)
ExpandImage: Expand image
ExpandImageCenter: in the center
ExpandImageInside: inside post
YoutubeLoadTitles: Load titles
YoutubeLoadThumbs: Load thumbnails
YoutubeWidth: Width
YoutubeHeight: Height
RecentIgnoredBoards: /~/ ignored boards

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

ModlogStickThread thr@Int board@Text: Sticked thread ##{show thr} on /#{board}/
ModlogLockThread thr@Int board@Text: Locked thread ##{show thr} on /#{board}/
ModlogAutosageThread thr@Int board@Text: Turned off bumping thread ##{show thr} on /#{board}/

ModlogHellban uid@Text: Hellbanned user #{uid}
ModlogUnhellban uid@Text: Unhellbanned user #{uid}
