######################################################################################
#### Post form
######################################################################################
## fields
Name: Name
Subject: Post's subject
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
ImgsWillBeThumbnailed s@Int: Images greater than #{show s}x#{show s} will be thumbnailed.
OpModerationIsEnabled: OP moderation is enabled on this board.
PostFormBumpLimit b@Int: Bump limit is #{unlimitedBump b "∞"}
### misc
ThreadHeaderNumber t@Int: Thread №#{show t}
ThreadHeaderReplyTo t@Int: Reply to #{show t} thread
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
EditingHistoryOfPost desu@Int: Editing history of post №#{show desu}
YouAlreadyEditedPost boku@Int: You've already edited this post #{show boku} times.
EditPosts: Edit posts and view history

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

Error lolka@Text: Error: #{lolka}
MustBeGreaterThan qwe@Text qw@Int: '#{qwe}' must be greater than #{show qw}
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
BoardThreadAccess: To crate new thread you must be a
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
PasswordChanged: Password successully changed

### Users management
Users: Users
AddNewUserOrUpdate: New user/update
## Messages
UsersAddedOrUpdated: New user successufully added/updated
UsersDeleted: User successufully deleted
UserDoesNotExist: User doesn't exist
YouAreTheOnlyWhoCanManageUsers: You are the only who can manage users!

### Config management
Config: Config
ShowNews: How many news show
ShowLatestPosts: How many latest posts show on /feed
MaxEditings: How many times you can edit your post
NewsBoard: Board for news
UpdateConfig: Update config
ReplyDelay: Reply delay
ThreadDelay: New thread delay
## Messages
ConfigUpdated: Config successufly updated

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
######################################################################################
#### Settings
######################################################################################
Settings: Settings
Apply: Apply
Applied: Settings applied
TimeZone: Time zone
Style: Style
Language: Language