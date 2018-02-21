module Language.ATS.Package.Build.IO ( configure
                                     , make
                                     , install
                                     , clibSetup
                                     , maybeExit
                                     , pkgHome
                                     , allSubdirs
                                     ) where

import           Development.Shake.ATS
import           Quaalude

pkgHome :: CCompiler -> IO FilePath
pkgHome cc' = (++ ("/.atspkg/" ++ ccToDir cc')) <$> getEnv "HOME"

allSubdirs :: FilePath -> IO [FilePath]
allSubdirs [] = pure mempty
allSubdirs d = do
    d' <- listDirectory d
    let d'' = ((d <> "/") <>) <$> d'
    ds <- filterM doesDirectoryExist d''
    ds' <- mapM allSubdirs ds
    pure $ join (ds : ds')

maybeExit :: ExitCode -> IO ()
maybeExit ExitSuccess = pure ()
maybeExit x           = exitWith x

clibSetup :: CCompiler -- ^ C compiler
          -> String -- ^ Library name
          -> FilePath -- ^ Filepath to unpack to
          -> IO ()
clibSetup cc' lib' p = do

    -- Find configure script and make it executable
    subdirs <- allSubdirs p
    configurePath <- fromMaybe (p <> "/configure") <$> findFile subdirs "configure"
    setFileMode configurePath ownerModes

    -- Set environment variables for configure script
    h <- pkgHome cc'
    let procEnv = Just [("CC", ccToString cc'), ("CFLAGS" :: String, "-I" <> h <> "include"), ("PATH", "/usr/bin:/bin")]

    biaxe [configure h configurePath procEnv, make, install] lib' p

configure :: FilePath -> FilePath -> Maybe [(String, String)] -> String -> FilePath -> IO ()
configure prefixPath configurePath procEnv lib' p =
    putStrLn ("configuring " ++ lib' ++ "...") >>
    silentCreateProcess ((proc configurePath ["--prefix", prefixPath, "--host", host]) { cwd = Just p, env = procEnv })

make :: String -> FilePath -> IO ()
make lib' p =
    putStrLn ("building " ++ lib' ++ "...") >>
    silentCreateProcess ((proc "make" []) { cwd = Just p })

install :: String -> FilePath -> IO ()
install lib' p =
    putStrLn ("installing " ++ lib' ++ "...") >>
    silentCreateProcess ((proc "make" ["install"]) { cwd = Just p })
