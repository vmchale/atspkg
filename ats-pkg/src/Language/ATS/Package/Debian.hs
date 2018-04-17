{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Language.ATS.Package.Debian ( debRules
                                   , Debian (..)
                                   ) where

import           Data.Dependency            (Version (..))
import           Data.Hashable              (Hashable)
import           Data.List                  (intercalate)
import           Data.Text.Lazy             (Text)
import           Development.Shake
import           Development.Shake.FilePath
import           Dhall
import           Quaalude

data Debian = Debian { package     :: Text
                     , version     :: Version
                     , maintainer  :: Text
                     , description :: Text
                     , target      :: Text
                     , manpage     :: Maybe Text
                     , binaries    :: [Text]
                     -- , libraries :: [Text]
                     }
                     deriving (Generic, Hashable, Binary, Interpret)

deriving newtype instance Interpret Version
deriving newtype instance Hashable Version

control :: Debian -> String
control Debian{..} = intercalate "\n"
    [ "Package: " ++ unpack package
    , "Version: " ++ show version
    , "Architecture: all"
    , "Maintainer: " ++ unpack maintainer
    , "Description: " ++ unpack description
    , mempty
    ]

-- look at hackage package for debian?
debRules :: Debian -> Rules ()
debRules deb =
    unpack (target deb) %> \out -> do
        need (unpack <$> binaries deb)
        let packDir = unpack (package deb)
            makeRel = (("target/" ++ packDir ++ "/") ++)
            debianDir = makeRel "/DEBIAN"
            binDir = makeRel "/usr/local/bin"
            manDir = makeRel "/usr/local/share/man/man1"
        mapM_ (liftIO . createDirectoryIfMissing True)
            [ binDir, debianDir, manDir ]
        fold $ do
            mp <- manpage deb
            pure $
                need [unpack mp] >>
                copyFile' (unpack mp) (manDir ++ "/" ++ takeFileName (unpack mp))
        zipWithM_ copyFile' (unpack <$> binaries deb) (((binDir ++ "/") ++) . unpack <$> binaries deb)
        writeFileChanged (debianDir ++ "/control") (control deb)
        command [Cwd "target"] "dpkg-deb" ["--build", packDir, dropDirectory1 out]
