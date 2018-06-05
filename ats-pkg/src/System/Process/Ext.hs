module System.Process.Ext ( silentCreateProcess
                          ) where

import           Control.Monad
import           Development.Shake
import           System.Exit
import           System.Process

verbosityErr :: Verbosity -> StdStream
verbosityErr v | v >= Loud = Inherit
verbosityErr _ = CreatePipe

handleExit :: ExitCode -> IO ()
handleExit ExitSuccess = mempty
handleExit x           = exitWith x

silentCreateProcess :: Verbosity -> CreateProcess -> IO ()
silentCreateProcess v proc' | v >= Chatty = do
    (_, _, _, r) <- createProcess (proc' { std_err = verbosityErr v, std_out = Inherit })
    handleExit =<< waitForProcess r
silentCreateProcess v proc' = void $ readCreateProcess (proc' { std_err = verbosityErr v }) ""
