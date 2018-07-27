module Distribution.ATS.Version ( atspkgVersion
                                ) where

import qualified Data.Version  as V
import qualified Paths_ats_pkg as P

atspkgVersion :: V.Version
atspkgVersion = P.version
