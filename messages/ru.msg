######################################################################################
#### Post form
######################################################################################
## fields
Name: Имя
PostSubject: Тема поста
ThreadSubject: Тема треда
SubmitBtn: Отправить
Message: Текст сообщения
Password: Пароль
File: Файл
Goback: Перейти к
ToThread: треду
ToBoard: борде
ToFeed: /feed/
Nobump: Не бампать
Captcha: Проверочный код

### Post form rules
SupportedFileTypes: Поддерживаемые типы файлов:
MaxFileSize: Маскимальный размер файлов:
ImgsWillBeThumbnailed size@Int: Изображения больше #{show size}x#{show size} будут уменьшены.
OpModerationIsEnabled: На этой доске включена ОП-модерация.
PostFormBumpLimit bl@Int: Бамп-лимит: #{unlimitedBump bl "∞"}
### misc
ThreadHeaderNumber thr@Int: Тред №#{show thr}
ThreadHeaderReplyTo thr2@Int: Ответ в тред №#{show thr2}
NewThread: Новый тред
ClosePostForm: Закрыть форму
Reply: Ответить
ReplyToThread: Ответ в тред №
AttachFile: Прикрепить файл
ShowRules: Показать правила
HideRules: Скрыть правила

### Editing
Edit: Редактировать
PostEdited: Пост отредактирован
PostNotYours: Неверный пароль или сессия. Вы не можете редактировать этот пост.
PostEditingIsDisabled: Редактирование постов отключено на этой борде
ThreadEditingIsDisabled: Редактирование ОП-постов отключено на этой борде
LastModified: Последнее изменение
EditingHistory: История измнений
EditingHistoryIsDisabled: Просмотр истории изменений отключен
GoTo: Перейти к
EditingHistoryOfPost post@Int: История изменений поста №#{show post}.
YouAlreadyEditedPost n@Int: Вы уже отредактировали пост #{show n} #{timesRus n}. Больше нельзя.

### Markup
PlainText: Простой текст
MarkupBold: жирный
MarkupItalic: курсив
MarkupUnderline: подчеркнутый
MarkupStrike: зачеркнутый
MarkupSpoiler: спойлер
MarkupCode: код ▼
MarkupInlineLatex: inline LaTeX
MarkupLatex: LaTeX
MarkupQuote: >цитата

Error err@Text: Ошибка: #{err}
MustBeGreaterThan thing@Text n@Int: '#{thing}' должно быть больше #{show n}
######################################################################################
#### Page
######################################################################################
News: Новости
Feed: Лента постов
Iwantmore: Хочу еще
## thread
Open: Открыть
PostsOmitted n@Int: #{show n} #{omittedRus n}.
ExpandThread: Развернуть тред.
ThreadIsExpanded: Тред развернут.
TruncateThread: Свернуть.
NoThreads: Нет тредов
## pages
Pages: Страницы:
Back: Назад
Next: Вперед
Previous: Назад
DeleteBtn: Удалить
OnlyFiles: Только файлы
## panel
Manage: Управление
Login: Войти
Logout: Выйти
Home: Домой
### admin buttons
BanPoster: Забанить!
StickThread: Прикрепить
UnstickThread: Открепить
LockThread: Заблокировать
UnlockThread: Разблокировать
Autosage: Автосажа
ThreadIsSticked: Тред закреплен
ThreadIsLocked: Тред закрыт
ThreadAutosage: Тред не бампается
### Deleted posts
DeletedPosts: Удаленные посты
DeletedPostsIn thr@Int: Удаленные посты в треде №#{show thr}
ShowDeletedPosts: Просмотреть удаленные посты

######################################################################################
#### Posting messages
######################################################################################
WrongCaptcha: Неверная капча
NoFileOrText: Необходимо ввести текст или прикрепить файл
NoFile: Необходимо приложить файл
OpFileIsDisabled: Тред должен быть без файлов
ThreadTitleIsRequired: Необходимо ввести тему треда
ReplyFileIsDisabled: Ответ в тред должен быть без файлов
UnknownError: Неизвестная ошибка
BadFormData: Что-то введено не так
NoFormData: Ничего не введено

TooLongMessage maxl@Int: Сообщение слишком длинное. Максимальная длина #{show maxl}
DeleteNoPosts: Не отмечено постов к удалению
DeleteWrongPassword: Неправильный пароль
YouAreNotOp: Пароль/сессия не совпадает с тем, что в ОП-посте. Вы не можете модерировать этот тред.
TypeNotAllowed: Данный тип файла не разрешен к загрузке
NoSuchThread: Нет такого треда
NoSuchBoard: Нет такой борды
LockedThread: Тред закрыт
PostSent: Пост отправлен
PostingTooFast: Вы постите слишком часто. Упырьте мел.

######################################################################################
#### Management
######################################################################################
Management: Управление
BanManagement: Баны
BoardManagement: Настройки досок
Enable: Включено
Disable: Выключено
Required: Обязательно
XForUnlim x@Text: #{x} — неограничено
Boards: Борды
Bans: Баны
MoveThread: Переместить тред
WhichBoard: На какую борду?
ChangeThread: Переместить пост в другой тред
WhichThread: В какой тред? 0 для создания нового треда


### Boards
BoardIndexNumber: Порядковый номер
BoardName: Имя борды
BoardTitle: Название борды
BoardTitleEG: e.g. бред, музыка
BoardSummary: Описание борды
BoardSummaryEG: e.g. "Доска обо всем"
BoardBumpLimit: Бамп-лимит
BoardNumberFiles : Макс. число файлов
BoardAllowedTypes: Типы файлов
BoardExtraRules: Дополнительные правила
BoardDefaultName : Стандартное имя
BoardMaxMsgLength: Макс. длина сообщения
BoardThumbSize : Максимальный размер превью в пикселях
BoardThreadsPerPage: Тредов на странице
BoardPreviewsPerThread: Превью-постов
BoardOpFile: Файл в ОП-посте
BoardReplyFile: Файл в ответе
BoardHidden: Скрытая
BoardThreadLimit: Максимум тредов
BoardEnableCaptcha: Капча
BoardEnableGeoIp: Отображать флаги стран
BoardCategory: Категория
BoardCategories: Категории досок (через запятую)
BoardViewAccess: Для просмотра доски нужно быть
BoardReplyAccess: Для ответа в тред нужно быть
BoardThreadAccess: Для создание треда нужно быть
BoardEnableForcedAnon: Принудительная анонимность (отключение ввода имени)
BoardRequiredThreadTitle: Обязательное название треда
WontBeUpdatedIfNone: Эти настройки не будут обновлены при значении <None>
OpModeration: ОП-модерация
PostEditing: Редактирование постов
OpEditing: Редактирование ОП-постов
ShowEditHistory: Показывать историю изменений
ShowPostDate: Показывать дату постов
## board list
NewBoard: Новая борда
AllBoards: Все борды
## actions
UpdateBoardsBtn: Обновить
AddBoardBtn: Добавить
DeleteAll: Удалить все
CleanBoard: Очистить
CleanBoardConfirm: Будут удалены ВСЕ посты на борде. Точно продолжить?
DeleteBoardConfirm: Будет удалена борда вместе со всеми постами. Точно продолжить?
RebuildPostsMessagesOnBoard: Обновить HTML сообщений в постах
## info
SeparateBy d@Text: Разделять с помощью '#{d}'
SeparateBySpace: Разделять пробелом
Optional: Необязательно
## Messages
WrongBoardId: Неправильное ID борды
BoardsUpdated: Борды обновлены
BoardAdded: Борда добавлена
BoardCleaned: Борда очищена от всех постов
BoardDeleted: Борда удалена

### Bans management
Ban: Бан
BanIp: IP
BanReason: Причина
BanExpires: Истекает
BanExpiresInHours: Истекает через * часов
BanBoard: На борде
NeverExpires: НИКОГДА
## Messages
YouAreBanned reason@Text expires@Text: Вы забанены! Причина: "#{reason}". Бан истекает #{expires}
BanAdded: Бан добавлен
BanDeleted: Бан удален

### Groups management
Group: Группа
Groups: Группы
GroupAddOrUpdate: Добавить/обновить
GroupAddedOrUpdated: Группа добавлена/обновлена
GroupDoesNotExist: Группа не существует
GroupDeleted: Группа удалена

### Account
Account: Мой аккаунт
OldPassword: Старый пароль
NewPassword: Новый пароль
PostsSent: Постов отправлено
ChangePassword: Сменить
## Messages
PasswordChanged: Пароль изменен
WrongPassword: Неправильный пароль

### Users management
Users: Пользователи
AddNewUserOrUpdate: Новый аккаунт/обновить
## Messages
UsersAdded: Аккаунт добавлен
UsersUpdated: Аккаунт обновлен
UsersPasswordNotSpecified: Не указан пароль
UserDoesNotExist: Пользователь не существует
UsersDeleted: Аккаунт удален
UsersDeleteConfirm: Точно удалить?
YouAreTheOnlyWhoCanManageUsers: Нетъ. Вы единственный, кто может добавить нового пользователя.

### Config management
Config: Настройки
ShowNews: Сколько показывать новостей
ShowLatestPosts: Сколько показывать последних постов в /feed
MaxEditings: Сколько раз можно редактировать пост
NewsBoard: Доска для новостей
UpdateConfig: Обновить настройки
ReplyDelay: Задержка перед ответом в тред
ThreadDelay: Задержка перед созданием нового треда
AdminAboutPage: Html страницы about
AdminHomePage: Html домашней страницы
ModlogMaxEntriesView: Максимальное количество записей в мод-логе
ModlogEntriesPerPage: Сколько показывать мод-лог записей на странице
## Messages
ConfigUpdated: Настройки обновлены

### Permissions
Permissions: Права
ManageThread: Управление тредами
ManageBoard : Управление бордами
ManageUsers : Управление пользователями
ManageConfig: Изменение конфига
DeletePosts : Удаление постов
ManagePanel : Доступ в админку
ManageBan   : Бан
AdditionalMarkup: Расширенная разметка
EditPosts: Редактирование постов и просмотр истории
ShadowEdit: Редактирование постов без сохранения истории
AppControl: Управление приложением
ViewModlog: Просматривать мод-логи 
ViewIPAndID: Просматривать IP и ID постеров
Hellbanning: Премодерация
ChangeFileRating: Изменять рейтинг цензуры у файлов 

DontSaveHistory: Не сохранять историю изменений
######################################################################################
#### Hellbanning
######################################################################################
HellbanHide: Скрыть пост для всех кроме его автора
HellbanPoster: Премодерировать все дальнейшие сообщения автора
HideAndHellban: Скрыть этот пост и премодерировать дальнейшие сообщения его автора
HideAllAndHellban: Скрыть все посты и премодерировать дальнейшие сообщения их автора

HellbanShowPost: Показавать этот пост для всех
HellbanUnbanUser: Снять пользователя с премодерации
HellbanShowAndUnban: Показать пост и снять пользователя с премодерации

PostIsVisibleOnlyForAuthor: Этот пост виден только его автору

HellbanAllPosts: Все премодерируемые посты

######################################################################################
#### Help
######################################################################################
Help: Справка
## Markup
Markup: Разметка
IfYouAreOp: Если вы ОП
IfYouAreNotOp: Если вы не ОП
IfPostBelongs: Если данный пост принадлежит вам
IfPostDoesntBelong: Если данный пост не принадлежит вам
ProofLabels: Пруфметки
Latex: LaTeX
SourceCode: Программный код
SupportedLanguages: Поддерживаемые языки для тега [code=<язык>]

######################################################################################
#### Ajax, popups, refmap, ...
######################################################################################
Online: Онлайн
GetNewPosts: Получить новые посты (если есть)
NoNewPosts: Нет новых постов
NewPosts: Новые посты!

ThreadNotExist: Похоже, этот тред больше не существует
NoPostsAnymore: Внезапно, в этом треде постов нет вообще
Loading: Загрузка...
ConnectionError: Ошибка соединения

Replies: Ответы:
CloseAllPopupIcon: Закрыть все
CloseOnePopupIcon: Закрыть

MarkToDeleteIcon: Отметить к удалению
EditIcon: Редактировать
ReplyIcon: Ответить
RemoveFileIcon: Убрать файл
CloseIcon: Закрыть

PostMessageIsTooLong: Сообщение слишком длинное.
ExpandPost: Полный текст.
ShrinkPost: Обрезать текст.

MarkAsRead: Отметить как прочитанное
######################################################################################
#### Thread hiding
######################################################################################
HideThread: Скрыть тред
ThreadIsHidden: Тред скрыт.
HiddenThreads: Скрытые треды
NoHiddenThreads: Нет скрытых тредов
ShowThread: Показать тред

######################################################################################
#### Settings
######################################################################################
Settings: Настройки
Apply: Применить
Applied: Настройки применены
TimeZone: Часовой пояс
Style: Стиль
Language: Язык
FeedIgnoredBoards: /feed/ игнорируемые борды
MaxAllowedRating: Максимальный рейтинг
ResetDefaults: Сбросить
JSFeatures: JavaScript фичи

######################################################################################
#### Modlogs
######################################################################################
Modlog: Мод-логи
ModlogDate: Дата
ModlogGroup: Группа
ModlogUser: Пользователь
ModlogMessage: Сообщение

ModlogBanAdded ip@Text reason@Text nn@Int: Забанен IP #{ip} — ##{show nn} — #{reason}
ModlogDelBan nnn@Int: Удален бан ##{show nnn}

ModlogNewBoard bn@Text: Добавлена борда /#{bn}/
ModlogUpdateAllBoards: Обновлены все борды
ModlogUpdateBoard bnn@Text: Обновлена борда /#{bnn}/
ModlogCleanAllBoards: Очищены все борды
ModlogCleanBoard bnnn@Text: Очищена борда /#{bnnn}/
ModlogDeleteAllBoards: Удалены все борды
ModlogDeleteBoard adsf@Text: Удалена борда /#{adsf}/

ModlogUpdateGroup ggp@Text: Обновлена группа #{ggp}
ModlogAddGroup gag@Text: Добавлена группа #{gag}
ModlogDelGroup gsd@Text: Удалена группа #{gsd}

ModlogAddUser gag@Text: Добавлен пользователь #{gag}
ModlogDelUser gag@Text: Удален пользователь #{gag}
ModlogChangeUserPassword g@Text: Изменен пароль пользователя #{g}
ModlogChangeUserGroup u@Text g@Text: Изменена группа пользователя #{u} на #{g}

ModlogUpdateConfig: Обновлен конфиг

ModlogStickThread thr1@Text: Прикреплен тред #{thr1}
ModlogLockThread thr1@Text: Закрыт постинг в треде #{thr1}
ModlogAutosageOnThread thr1@Text: Отключен бамп треда #{thr1}

ModlogUnstickThread thr1@Text: Откреплен тред #{thr1}
ModlogUnlockThread thr1@Text: Разблокирован тред #{thr1}
ModlogAutosageOffThread thr1@Text: Включен бамп треда #{thr1}

ModlogHellbanHidePost p@Text: Премодерация: скрыт пост #{p}
ModlogHellbanShowPost p@Text: Премодерация: показан пост #{p}

ModlogHellban uid@Text: Премодерируются все сообщения пользователя #{uid}
ModlogUnhellban uid@Text: Пользователь снят с премодерации #{uid}

ModlogHellbanHideAllPosts uid@Text: Премодерация: скрыть все посты пользователя #{uid}
ModlogHellbanShowAllPosts uid@Text: Премодерация: показать все посты пользователя #{uid}

ModlogRebuildPostsMessagesOn board@Text: Обновлены HTML сообщения в постах на борде /#{board}/
ModlogRebuildPostsMessages: Обновлены HTML сообщения в постах на борде

ModlogMoveThread from@Text to@Text: Перемещен тред из #{from} в #{to}
ModlogChangeThread from@Text to@Text p@Text: Перемещен #{p} пост из треда #{from} в тред #{to}

NoSuchPost: Нет такого поста

ModlogChangeRating f@Text p@Text r1@Text r2@Text: Смена рейтинга файла #{f}  #{r1} → #{r2} #{p}

ModlogDisablePostEditing p@Text: Отключено редактирование поста #{p}
ModlogEnablePostEditing p@Text: Включено редактирование поста #{p}

ModlogRecoverPosts p@Text: Восстановлены удаленные посты #{p}

ModlogDeletePosts p@Text: Удалены посты #{p}
ModlogDeleteFile f@Text p@Text: Удален файл #{f} поста #{p}

ModlogLogin: Залогинился
ModlogLogout: Вышел

ModlogAppRestart: Перезапустил движок
ModlogExecuteGitPull: Выполнил 'git pull'

######################################################################################
#### Admin search posts
######################################################################################
Search: Поиск
AdminSearchUID: Искать посты с этим ID пользователя
AdminSearchIP: Искать посты с этим IP
PostsByUserID: Все посты по ID пользователя
PostsByIP: Все посты с этим IP
DisplayOnlyHellbanned: Показывать только посты на премодерации
DisplayAllPosts: Показывать все посты
AllHellbannedUsers: Все пользователи на премодерации
######################################################################################
#### Admin deleted posts
######################################################################################
AdminDeletedPosts: Удаленнные посты
AdminAllDeletedPosts: Все удаленные посты
AdminRecoverDeletedPosts: Восстановить отмеченные посты
AdminRecoveredDeletedPosts: Удаленные посты восстановлены
AdminDeletedFromBoard: на борде 
AdminDeletedFromThread: в треде 
AdminDeletedFilter: Отфильтровать посты
AdminDeletedWhichBoard: с какой борды?
AdminDeletedWhichThread: из какого треда? 0 - показать только треды, -1 - со всех тредов
######################################################################################
ImageSource: Соус
PostPreview: Предварительный просмотр
DeleteFile: Удалить файл
DeleteFileConfirm: Файл нельзя будет восстановить. Продолжить?
FileNotYours: Неверная сессия. Файл не ваш и удалить его нельзя.
DisabledEditing: Редактирование этого поста отключено
LockEditing: Отключить редактирование этого поста для юзера
UnlockEditing: Включить редактирование этого поста для юзера
HomeBoards: Доски
######################################################################################
RssFeed s@Text b@Text: #{s} /#{b}/ RSS лента
RssRecentPosts b@Text: Последние посты с доски /#{b}/
SuccessEx: Успех!
ErrorEx: Ошибка!
AppRestart: Перезапустить движок
AppGitPull: Выполнить 'git pull'
MoveElement: Переместить
######################################################################################
## JS Settings
######################################################################################
DesktopNotification: Уведомления на рабочем столе
BoardStatsUpdate: Интервал обновления счетчиков новых постов в списке борд (мс.)
Eventsource: EventSource - автообновление отредактированных постов, мгновенное обновление счетика новых постов и пр.
ExpandFilesInPost: Разворачивать файлы в посте
PostsAutoloadInterval: Интервал обновления треда/фида (мс.)
Mathjax: Включить LaTeX
AnimationSpeed: Скорость анимации всплывающих сообщений (мс.)
PopupMessageDisplayTime: Время показа всплывающих сообщений (мс.)
PopupPostDelayBeforeDisplaying: Задержка перед отображением всплывающих постов (мс.)
PopupPostRemovingOnMouseOut: Включить удаление всплывающих постов по наведению курсора за границы поста, при выкл. — с помощью кнопок
PpopupPostDelayBeforeRemoving: Задержка перед удаление всплывающих постов (мс.)
VideoRememberVolume: Запоминать громкость видео 
VideoDefaultVolume: Стандартная громкость
VideoAutoplay: Автовоспроизведение видео при его разворачивании
VideoLoop: Зацикливать воспроизведение видео
VideoScaleWidth: Во сколько раз масштабировать видео по ширине при разворачивании
VideoScaleHeight: ... по высоте
VimeoEnable: Встраивать плеер вместо ссылки на vimeo
CoubEnable:  ... coub
YoutubeEnable: ... YouTube
YoutubeLoadTitles: Загружать названия видео вместо ссылки на YouTube
YoutubeHtml5: Включить HTML5 для YouTube плеера
UrlVideoWidth: Ширина плеера для видео из ссылок
UrlVideoHeight: Высота плеера для видео из ссылок
FlashWidth: Ширина развернутых флеш-файлов
FlashHeight: Высота развернутых флеш-файлов
IframelyAPIKey: iframely API key
YoutubeAPIKey: youtube API key
CssWidePosts: Растягивать посты по ширине
CssSideMargin: Отступ по бокам страницы в %, 0 - стандартный отступ
CssHideFilesNames: Скрывать имена файлов
CssHideFilesInfo: Скрывать информацию о файлах
CssHideMarkupButtons: Скрывать кнопки разметки
CssCustom: Пользовательский CSS
