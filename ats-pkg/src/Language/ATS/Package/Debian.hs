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

import qualified Codec.Compression.GZip     as Gzip
import qualified Data.ByteString.Lazy       as BSL
import           Data.Dependency            (Version (..))
import           Data.List                  (intercalate)
import           Development.Shake          hiding ((*>))
import           Development.Shake.FilePath
import           Dhall                      hiding (Text)
import           Quaalude
import           System.PosixCompat.Files   (setFileMode)

data Debian = Debian { package     :: Text
                     , version     :: Version
                     , maintainer  :: Text
                     , description :: Text
                     , target      :: Text
                     , manpage     :: Maybe Text
                     , binaries    :: [Text]
                     , libraries   :: [Text]
                     , headers     :: [Text]
                     , license     :: Maybe Text
                     , changelog   :: Maybe Text
                     }
                     -- TODO: section https://www.debian.org/doc/debian-policy/ch-archive.html#s-subsections
                     -- TODO: priority https://www.debian.org/doc/debian-policy/ch-archive.html#s-priorities
                     deriving (Generic, Binary, FromDhall)

deriving newtype instance FromDhall Version

control :: Debian -> String
control Debian{..} = intercalate "\n"
    [ "Package: " ++ unpack package
    , "Version: " ++ show version
    , "Architecture: all"
    , "Maintainer: " ++ unpack maintainer
    , "Description: " ++ unpack description
    , mempty
    ]

debianCompress :: BSL.ByteString -> BSL.ByteString
debianCompress = Gzip.compressWith Gzip.defaultCompressParams { Gzip.compressLevel = Gzip.bestCompression }

gzipRules :: Rules ()
gzipRules =
    "//*.gz" %> \out -> do
        let orig = fromMaybe out $ stripExtension "gz" out
        need [orig]
        contents <- liftIO $ BSL.readFile orig
        let zipped = debianCompress contents
        liftIO $ BSL.writeFile out zipped


-- look at hackage package for debian?
debRules :: Debian -> Rules ()
debRules deb = do

    gzipRules -- TODO: right place?

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

        traverse_ (\fp -> liftIO $ setFileMode fp binPerms)
            binaries'

        let dirs = [ binDir, debianDir, manDir, includeDir, docDir ]

        traverse_ (liftIO . createDirectoryIfMissing True) dirs

        let parents = [ "usr", "usr/share/man", "usr/share", "usr/share/doc" ]

        traverse_ (\fp -> liftIO $ setFileMode fp binPerms)
            ((makeRel <$> parents) ++ dirs)

        fold $ do
            mp <- manpage deb
            pure $ do
                let mp' = unpack mp <.> "gz"
                need [mp']
                liftIO (setFileMode mp' manPerms)
                copyFile' mp' (manDir ++ "/" ++ takeFileName mp')

        let moveFiles files dir = zipWithM_ copyFile' files ((dir </>) . takeFileName <$> files)

        moveFiles binaries' binDir
        moveFiles libraries' libDir
        moveFiles headers' includeDir

        writeFileChanged (debianDir ++ "/control") (control deb)

        command [Cwd "target"] "dpkg-deb" ["--build", packDir, dropDirectory1 out]
