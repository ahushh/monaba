######################################################################################
#### Post form
######################################################################################
## fields
Name: Nome
PostSubject: Assunto do post
ThreadSubject: Assunto da thread
SubmitBtn: Enviar
Message: Seu texto
Password: Password
File: Arquivo
Goback: Voltar
ToThread: thread
ToBoard: board
ToFeed: /feed/
Nobump: Sage
Captcha: Captcha

### Post form rules
SupportedFileTypes: Formatos permitidos:
MaxFileSize: Tamanho máximo:
ImgsWillBeThumbnailed size@Int: Imagens maiores que #{show size}x#{show size} serão miniaturizadas.
OpModerationIsEnabled: OP Moderador está ativo nessa board.
PostFormBumpLimit bl@Int: Bump limit: #{unlimitedBump bl "∞"}
### misc
ThreadHeaderNumber thr@Int: Thread ##{show thr}
ThreadHeaderReplyTo thr2@Int: Reply em #{show thr2} thread
NewThread: Nova thread
ClosePostForm: Fechar
Reply: Reply
ReplyToThread: Reply na thread №
AttachFile: Anexar
ShowRules: Exibir
HideRules: Esconder

### Editing
Edit: Editar
PostEdited: Post editado com sucesso
PostNotYours: Senha ou sessão inválida. Você não pode editar esse post.
PostEditingIsDisabled: Edição de post está desativada nessa board.
ThreadEditingIsDisabled: Edição OP está desativada nessa board.
LastModified: Última modificação
EditingHistory: Histórico de modificações
EditingHistoryIsDisabled: Histórico de modificações está desativado
GoTo: Ir para
EditingHistoryOfPost post@Int: Histórico de modificações do post ##{show post}
YouAlreadyEditedPost n@Int: Você já editou esse post #{show n} vezes.

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
MustBeGreaterThan thing@Text n@Int: '#{thing}' deve ser maior que #{show n}
######################################################################################
#### Page
######################################################################################
News: News
Feed: Feed
Iwantmore: MOAR
## thread
Open: Abrir
PostsOmitted n@Int: #{show n} #{plural n "post" "posts"} oculto.
ExpandThread: Expandir thread.
ThreadIsExpanded: Thread está expandida.
TruncateThread: Trancada.
NoThreads: Sem threads ainda
## pages
Pages: Páginas:
Back: Voltar
Next: Próximo
Previous: Anterior
DeleteBtn: Delete
OnlyFiles: Somente arquivos
## panel
Manage: Manage
Login: Login
Logout: Logout
Home: Home
### admin buttons
BanPoster: Ban poster
StickThread: Pinar/despinar
LockThread: Trancar/destrancar
Autosage: Autosage
ThreadIsSticked: Thread está pinada
ThreadIsLocked: Thread está trancada
ThreadAutosage: Thread está em autosage
### Deleted posts
DeletedPosts: Posts deletados
DeletedPostsIn thr@Int: Posts deletados na thread ##{show thr}
ShowDeletedPosts: Mostrar posts deletados

######################################################################################
#### Posting messages
######################################################################################
WrongCaptcha: Captcha errado
NoFileOrText: É necessário arquivo ou texto
NoFile: É necessário um arquivo
OpFileIsDisabled: Anexo do OP está desativado nessa board
ThreadTitleIsRequired: É necessário um assunto para a thread
ReplyFileIsDisabled: Responder com anexo está desativado nessa board
UnknownError: Erro desconhecido
BadFormData: Erro ao processar os dados
NoFormData: Nenhum dado enviado
TooLongMessage maxl@Int: Sua mensagem é muito longa. Máximo de caracteres: #{show maxl}
DeleteNoPosts: Você não se esqueceu de marcar o post que deseja deletar?
DeleteWrongPassword: Password errado
YouAreNotOp: Password ou sessão do OP errado. Você não pode moderar essa thread.
TypeNotAllowed: Arquivo não permitido
NoSuchThread: Thread inexistente
NoSuchBoard: Board inexistente
LockedThread: Essa thread está tracanda e você não pode postar!
PostSent: Postagem realizada com sucesso!
PostingTooFast: Calma, anão. Beba um copo de água, você está postando rápido demais!

######################################################################################
#### Management
######################################################################################
Management: Administração
BanManagement: Administrar bans
BoardManagement: Administrar boards
Enable: Habilitado
Disable: Desativado
Required: Requerido
XForUnlim x@Text: #{x} para ilimitado
Boards: Boards
Bans: Bans
MoveThread: Mover thread
WhichBoard: Qual board?
ChangeThread: Mover post para outra thread
WhichThread: Qual thread? Insira 0 para criar nova thread

### Boards
BoardIndexNumber: Número index
BoardName: Nome de board
BoardTitle: Título
BoardTitleEG: ex: random, música
BoardSummary: Descrição da board
BoardSummaryEG:
BoardBumpLimit: Bump limit
BoardNumberFiles : Número máximo de arquivos
BoardAllowedTypes: Formatos permitidos
BoardExtraRules: Regras extras
BoardDefaultName : Nome default
BoardMaxMsgLength: Numéro máximo de caracteres
BoardThumbSize : Tamanho máximo da thumb em pixels
BoardThreadsPerPage: Threads por página
BoardPreviewsPerThread: Previews por página
BoardOpFile: OP file
BoardReplyFile: Reply file
BoardHidden: Oculta
BoardThreadLimit: Max threads
BoardEnableCaptcha: Habilitar captcha
BoardEnableGeoIp: Mostra country flags
BoardCategory: Categoria
BoardCategories: Categorias de board (separado por vírgula)
BoardViewAccess: Para visualizar essa board você deve ser
BoardReplyAccess: Para responder essa board você deve ser
BoardThreadAccess: Para criar nova thread você deve ser
BoardEnableForcedAnon: Forçar anonimato (Desativa o campo nome)
BoardRequiredThreadTitle: Exigir título da thread
WontBeUpdatedIfNone: Essas configurações não serão atualizadas se seus valores forem <None>
OpModeration: OP Moderador
PostEditing: Editar posts
OpEditing: Edição do OP
ShowEditHistory: Mostrar histórico de modificação
ShowPostDate: Mostrar data do post
## board list
NewBoard: Nova board
AllBoards: Todas as boards
## actions
UpdateBoardsBtn: Atualizar
AddBoardBtn: Adicionar board
DeleteAll: Deletar tudo
CleanBoard: Limpar
CleanBoardConfirm: Isso irá deletar TODOS os posts da board! Você tem certeza disso?
DeleteBoardConfirm: Você tem certeza que deseja deletar essa board?
RebuildPostsMessagesOnBoard: Rebuild HTML das mensagens do post
## info
SeparateBy d@Text: Separado por '#{d}'
SeparateBySpace: Separado por espaço
Optional: Opcional
## Messages
WrongBoardId: Board ID errada
BoardsUpdated: Boards atualizadas com sucesso
BoardAdded: Board adicionada com sucesso
BoardCleaned: Board limpada com sucesso
BoardDeleted: Board deletada com sucesso

### Bans management
Ban: Ban
BanIp: IP
BanReason: Motivo
BanExpires: Expiração
BanExpiresInHours: Expiração (em horas)
BanBoard: Board
NeverExpires: NUNCA
## Messages
YouAreBanned reason@Text expires@Text: Você foi banido! Motivo: "#{reason}". Seu ban expirará em #{expires}
BanAdded: Ban adicionado com sucesso
BanDeleted: Ban deletado com sucesso

### Groups management
Group: Grupo
Groups: Grupos
GroupAddOrUpdate: Adicionar/atualizar
GroupAddedOrUpdated: Adicionar/atualizar grupo
GroupDoesNotExist: Grupo não existe
GroupDeleted: Grupo deletado

### Account
Account: Conta
NewPassword: Novo password
## Messages
PasswordChanged: Password modificado com sucesso

### Users management
Users: Usuários
AddNewUserOrUpdate: Novo usuário/atualizar
## Messages
UsersDeleted: Usuário deletado com sucesso
UserDoesNotExist: Usuário não existe
YouAreTheOnlyWhoCanManageUsers: NÃO MERDA, SHERLOCK. Você é o único que pode administrar usuários.

### Config management
Config: Config
ShowNews: Quantas news serão mostradas
ShowLatestPosts: Quantos posts serão mostrados na página do /feed
MaxEditings: Quantas vezes você pode editar o post
NewsBoard: Board para news
UpdateConfig: Atualizar config
ReplyDelay: Reply delay
ThreadDelay: Nova thread delay
AdminAboutPage: Html da página about
AdminHomePage: Html da homepage
ModlogMaxEntriesView: Max de entradas no modlog
ModlogEntriesPerPage: Entradas de modlog por página
## Messages
ConfigUpdated: Config atualizada com sucesso

### Permissions
Permissions: Permissões
ManageThread:Administrar Thread
ManageBoard :Administrar Board
ManageUsers :Administrar Usuários
ManageConfig:Administrar Config
DeletePosts :Deletar Posts
ManagePanel :Administrar Painel
ManageBan   :Administrar Ban
AdditionalMarkup: Markup adicional
EditPosts: Editar posts e visualizar histórico
ViewModlog: Vizualizar log da moderação
ViewIPAndID: Vizualizar IP e ID dos usuários
Hellbanning: Hellbanning

######################################################################################
#### Hellbanning
######################################################################################
HellbanHide: Esconder esse post para qualquer usuário menos para o seu autor
HellbanPoster: Hellbanir esse usuário
HideAndHellban: Hide esse post e hellbanir o usuário
HideAllAndHellban: Hide todos os posts do usuário e o hellbanir

HellbanShowPost: Mostrar esse post para qualquer usuário
HellbanUnbanUser: Remover hellban esse usuário
HellbanShowAndUnban: Mostrar esse post remover hellban do usuário

PostIsVisibleOnlyForAuthor: Esse post é vísivel somente para o seu autor

HellbanAllPosts: Todos os posts escondidos

######################################################################################
#### Help
######################################################################################
Help: Help
## Markup
Markup: Marque
IfYouAreOp: Se você é o OP
IfYouAreNotOp: Se você não é o OP
IfPostBelongs: Se esse post pertence a você
IfPostDoesntBelong: Se esse post não pertence a você
ProofLabels: Proof labels
Latex: LaTeX
SourceCode: Source code
SupportedLanguages: Supported languages for tag [code=<language>]

######################################################################################
#### Ajax, popups, refmap, ...
######################################################################################
Online: Online:
GetNewPosts: Obter novos posts
NoNewPosts: Sem postos novos

ThreadNotExist: Essa thread não existe mais!
NoPostsAnymore: Ooops, sem posts nessa thread!
Loading: Carregando...
ConnectionError: Erro de conexão

Replies: Replies:
CloseAllPopupIcon: Fechar todos popups
CloseOnePopupIcon: Fechar

MarkToDeleteIcon: Marcar para deletar
EditIcon: Editar
ReplyIcon: Responder
RemoveFileIcon: Remover arquivo
CloseIcon: Fechar

PostMessageIsTooLong: Texto é muito longo.
ExpandPost: Mostra tudo.
ShrinkPost: Encolher.

MarkAsRead: Marcar como lido
######################################################################################
#### Thread hiding
######################################################################################
HideThread: Ocultar thread
ThreadIsHidden: Thread está oculta.
HiddenThreads: Threads ocultas
ShowThread: Mostrar threads

######################################################################################
#### Settings
######################################################################################
Settings: Configurações
Apply: Aplicar
Applied: Configurações aplicadas
TimeZone: Time zone
Style: Estilo
Language: Idioma
FeedIgnoredBoards: /feed/ boards ignoradas
ResetDefaults: Resetar para padrão

######################################################################################
#### Modlogs
######################################################################################
Modlog: Modlog
ModlogDate: Data
ModlogGroup: Grupo
ModlogUser: Usuário
ModlogMessage: Mensagem

ModlogBanAdded ip@Text reason@Text nn@Int: IP banido #{ip} — ##{show nn} — #{reason}
ModlogDelBan nnn@Int: Ban deletado ##{show nnn}

ModlogNewBoard bn@Text: Adicionou nova board /#{bn}/
ModlogUpdateAllBoards: Atualizou todas as boards
ModlogUpdateBoard bnn@Text: Atualizou board /#{bnn}/
ModlogCleanAllBoards: Limpou todas as boards
ModlogCleanBoard bnnn@Text: Limpo board /#{bnnn}/
ModlogDeleteAllBoards: Deletou todas as boards
ModlogDeleteBoard adsf@Text: Deletou board /#{adsf}/

ModlogUpdateGroup ggp@Text: Atualizou grupo #{ggp}
ModlogAddGroup gag@Text: Adicionou grupo #{gag}
ModlogDelGroup gsd@Text: Deletou grupo #{gsd}

ModlogAddUser gag@Text: Adicionou usuário #{gag}
ModlogDelUser gag@Text: Deletou usuário #{gag}

ModlogUpdateConfig: Atualizou config

ModlogStickThread thr1@Text: Pinou thread #{thr1}
ModlogLockThread thr1@Text: Trancou thread #{thr1}
ModlogAutosageThread thr1@Text: Desligou o bumping na thread #{thr1}

ModlogHellban uid@Text: Hellbanniu usuário #{uid}
ModlogUnhellban uid@Text: Removeu hellban do usuário #{uid}

ModlogRebuildPostsMessagesOn board@Text: Rebuilt HTML das mensagens do posts em /#{board}/
ModlogRebuildPostsMessages: Rebuilt HTML HTML das mensagens do posts

ModlogMoveThread from@Text to@Text: Moveu thread de #{from} para #{to}
ModlogChangeThread from@Text to@Text p@Text: Moveu #{p} da thread #{from} para #{to}

NoSuchPost: Postagem não encontrada

######################################################################################
#### Admin search posts
######################################################################################
Search: Search
AdminSearchUID: Buscar posts com esse ID
AdminSearchIP: Buscar posts com IP
PostsByUserID: Posts por ID
PostsByIP: Posts por IP
DisplayOnlyHellbanned: Mostrar somente hellbanned posts
DisplayAllPosts: Mostrar todos os posts
