module Import.NoFoundation
    ( module Import
    , filterBoards
    , sortBoards
    ) where

import Model                 as Import
import ModelTypes            as Import
import Prelude               as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod                 as Import
import Yesod.Auth            as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import
import Yesod.Static          as Import

import Control.Applicative          as Import (pure, (<$>), (<*>))
import Control.Concurrent.STM.TChan as Import
import Control.Concurrent.STM.TVar  as Import
import Control.Concurrent.STM       as Import (atomically)
import Control.Monad                as Import (unless, when, void, join, mplus, forM, forM_, foldM)
import Data.Maybe                   as Import (fromMaybe, fromJust, isJust, isNothing, mapMaybe, catMaybes)
import Data.Text                    as Import (Text, pack, unpack)
import Data.List                    as Import (nub, intercalate, sortBy)
import Data.Time                    as Import (UTCTime, getCurrentTime, utctDayTime, diffUTCTime)
import Text.Blaze.Html              as Import (preEscapedToHtml)
import Data.Monoid                  as Import (Monoid (mappend, mempty, mconcat), (<>))
import Database.Persist.Sql         as Import (SqlBackend)
---------------------------------------------------------------------------------------------------------
-- Template helpers
---------------------------------------------------------------------------------------------------------
import qualified Data.Text   as T

filterBoards :: [Entity Board] -> Text -> Maybe Text -> [Entity Board]
filterBoards boards category group = filter p boards
  where p (Entity _ b)  = notHidden b && checkCategory b && checkAccess b
        notHidden     b = not $ boardHidden b
        checkCategory b | T.null category = isNothing $ boardCategory b
                        | otherwise       = Just category == boardCategory b
        checkAccess   b = isNothing (boardViewAccess b) || (isJust group && elem (fromJust group) (fromJust $ boardViewAccess b))

sortBoards :: [Entity Board] -> [Entity Board]
sortBoards bs = sortBy c bs
  where c a b = let a' = entityVal a
                    b' = entityVal b
                    n1 = boardIndex a'
                    n2 = boardIndex b'
                in if n1 == n2
                   then (boardName $ entityVal a) `compare` (boardName $ entityVal b)
                   else n1 `compare` n2
