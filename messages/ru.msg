######################################################################################
#### Post form
######################################################################################
## fields
Name: Имя
Subject: Тема
SubmitBtn: Отправить
Message: Текст
Password: Пароль
File: Файл
Goback: Перейти к 
Nobump: Не бампать
ToThread: треду
ToBoard: борде
### Post form rules
SupportedFileTypes: Поддерживаемые типы файлов:
MaxFileSize: Маскимальный размер файла:
ImgsWillBeThumbnailed s@Int: Изображения больше #{show s}x#{show s} будут уменьшены.
OpModerationIsEnabled: На этой доске включена ОП-модерация.
PostFormBumpLimit b@Int: Бамп-лимит: #{unlimitedBump b "∞"}
### misc
ThreadHeaderNumber t@Int: Тред №#{show t}
ThreadHeaderReplyTo t@Int: Ответ в тред №#{show t}
NewThread: Новый тред
ClosePostForm: Закрыть форму
Reply: Ответить
ReplyToThread: Ответ в тред №

Edit: Редактировать
PostEdited: Пост отредактирован
PostNotYours: Неверный пароль или сессия. Вы не можете редактировать этот пост.
PostEditingIsDisabled: Редактирование постов отключено на этой борде
ThreadEditingIsDisabled: Редактирование ОП-постов отключено на этой борде
LastModified: Последнее изменение
EditingHistory: История измнений
EditingHistoryIsDisabled: Просмотр истории изменений отключен
GoTo: Перейти к
EditingHistoryOfPost desu@Int: История изменений поста №#{show desu}.
YouAlreadyEditedPost boku@Int: Вы уже отредактировали пост #{show boku} #{timesRus boku}. Больше нельзя.
EditPosts: Редактирование постов и просмотр истории

Error lolka@Text: Ошибка: #{lolka}
MustBeGreaterThan qwe@Text qw@Int: '#{qwe}' должно быть больше #{show qw}
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
StickThread: Прикрепить/открепить
LockThread: Заблокировать/разблокировать
Autosage: Автосажа
### Deleted posts
DeletedPosts: Удаленные посты
DeletedPostsIn z@Int: Удаленные посты в треде №#{show z}
ShowDeletedPosts: Просмотреть удаленные посты

######################################################################################
#### Posting messages
######################################################################################
NoFileOrText: Необходимо ввести текст или прикрепить файл
NoFile: Необходимо приложить файл
OpFileIsDisabled: Тред должен быть без файлов
ReplyFileIsDisabled: Ответ в тред должен быть без файлов
UnknownError: Неизвестная ошибка
BadFormData: Что-то введено не так
NoFormData: Ничего не введено

TooLongMessage n@Int: Сообщение слишком длинное. Максимальная длина #{show n}
DeleteNoPosts: Не отмечено постов к удалению
DeleteWrongPassword: Неправильный пароль
YouAreNotOp: Пароль/сессия не совпадает с тем, что в ОП-посте. Вы не можете модерировать этот тред.
TypeNotAllowed: Данный тип файла не разрешен к загрузк
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
WontBeUpdatedIfNone: Эти настройки не будут обновлены при значении <None>
OpModeration: ОП-модерация
PostEditing: Редактирование постов
OpEditing: Редактирование ОП-постов
ShowEditHistory: Показывать историю изменений
## board list
NewBoard: Новая борда
AllBoards: Все борды
## actions
UpdateBoardsBtn: Обновить
AddBoardBtn: Добавить
DeleteAll: Удалить все
CleanBoard: Очистить
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
InvalidData: Неверные данные
UpdateBoardsInvalidInput: Что-то введено не так

### Bans management
Ban: Бан
BanIp: IP
BanReason: Причина
BanExpires: Истекает
BanExpiresInHours: Истекает через * часов
BanBoard: На борде
NeverExpires: Никогда
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
NewPassword: Новый пароль
## Messages
PasswordChanged: Пароль изменен

### Users management
Users: Пользователи
AddNewUserOrUpdate: Новый аккаунт/обновить
## Messages
UsersAddedOrUpdated: Аккаунт добавлен/обновлен
UserDoesNotExist: Пользователь не существует
UsersDeleted: Аккаунт удален
YouAreTheOnlyWhoCanManageUsers: Вы единственный, кто может добавить нового пользователя!

### Config management
Config: Настройки
ShowNews: Сколько показывать новостей
ShowLatestPosts: Сколько показывать последних постов в /feed
MaxEditings: Сколько раз можно редактировать пост
NewsBoard: Доска для новостей
UpdateConfig: Обновить настройки
ReplyDelay: Задержка перед ответом в тред
ThreadDelay: Задержка перед созданием нового треда
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
GetNewPosts: Получить новые посты (если есть)
NoNewPosts: Нет новых постов

ThreadNotExist: Похоже, этот тред больше не существует
NoPostsAnymore: Внезапно, в этом треде постов нет вообще
Loading: Загрузка...

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
######################################################################################
#### Settings
######################################################################################
Settings: Настройки
Apply: Применить
Applied: Настройки применены
TimeZone: Часовой пояс
Style: Стиль
Language: Язык