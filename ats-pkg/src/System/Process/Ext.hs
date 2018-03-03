module System.Process.Ext ( silentCreateProcess
                          ) where

import           Control.Monad
import           System.Process

silentCreateProcess :: CreateProcess -> IO ()
silentCreateProcess proc' =
    void $ readCreateProcess (proc' { std_err = CreatePipe }) ""
