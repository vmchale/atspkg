module Language.ATS.Package.Exec ( exec
                                 ) where

import           Language.ATS.Package

exec :: IO ()
exec =
    nuke >>
    fetchCompiler >>
    setupCompiler
