{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Language.ATS.Package.Debian ( debRules
                                   , Debian (..)
                                   ) where

import           Data.Dependency            (Version (..))
import           Data.List                  (intercalate)
import           Development.Shake          hiding ((*>))
import           Development.Shake.FilePath
import           Dhall                      hiding (Text)
import           Quaalude

data Debian = Debian { package     :: Text
                     , version     :: Version
                     , maintainer  :: Text
                     , description :: Text
                     , target      :: Text
                     , manpage     :: Maybe Text
                     , binaries    :: [Text]
                     , libraries   :: [Text]
                     , headers     :: [Text]
                     }
                     deriving (Generic, Binary, Interpret)

deriving newtype instance Interpret Version

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

        let binPerms = 0o755
            manPerms = 0o0644

        let binaries' = unpack <$> binaries deb
            libraries' = unpack <$> libraries deb
            headers' = unpack <$> headers deb

        traverse_ need [ binaries'
                       , libraries'
                       , headers'
                       ]

        let packDir = unpack (package deb)
            makeRel = (("target" </> packDir) </>)
            debianDir = makeRel "DEBIAN"
            binDir = makeRel "usr/bin"
            libDir = makeRel "usr/lib"
            manDir = makeRel "usr/share/man/man1"
            includeDir = makeRel "usr/include"
            docDir = makeRel ("usr/share/doc" </> packDir)

        traverse_ (liftIO . createDirectoryIfMissing True)
            [ binDir, debianDir, manDir, includeDir, docDir ]

        fold $ do
            mp <- manpage deb
            pure $
                need [unpack mp] *>
                setFileMode mp manPerms
                copyFile' (unpack mp) (manDir ++ "/" ++ takeFileName (unpack mp))

        let moveFiles files dir = zipWithM_ copyFile' files ((dir </>) . takeFileName <$> files)

        moveFiles binaries' binDir
        moveFiles libraries' libDir
        moveFiles headers' includeDir

        writeFileChanged (debianDir ++ "/control") (control deb)

        command [Cwd "target"] "dpkg-deb" ["--build", packDir, dropDirectory1 out]
