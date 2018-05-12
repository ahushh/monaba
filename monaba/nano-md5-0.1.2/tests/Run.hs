-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.Run
-- Copyright   :  (c) Don Stewart 2006
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  experimental
-- Portability :  currently non-portable (Control.Concurrent)
--
-- Convenient interface to external processes 
--

module Run (

        -- * Running processes
        readProcess

    ) where

import System.Process
import System.Exit
import System.IO

import Control.Monad
import Control.Concurrent
import qualified Control.Exception as C

--
-- | readProcess forks an external process, reads its standard output,
-- waits for the process to terminate, and returns either the output
-- string, or an exitcode.
--
readProcess :: FilePath                     -- ^ command to run
            -> [String]                     -- ^ any arguments
            -> String                       -- ^ standard input
            -> IO (Either ExitCode String)  -- ^ either the stdout, or an exitcode

readProcess cmd args input = C.handle (return . handler) $ do

    (inh,outh,errh,pid) <- runInteractiveProcess cmd args Nothing Nothing

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    forkIO $ C.evaluate (length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ hPutStr inh input
    hClose inh          -- done with stdin
    hClose errh         -- ignore stderr

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- C.catch (waitForProcess pid) (\_ -> return ExitSuccess)

    return $ case ex of
        ExitSuccess   -> Right output
        ExitFailure _ -> Left ex

  where
    handler (C.ExitException e) = Left e
    handler e                   = Left (ExitFailure 1)

