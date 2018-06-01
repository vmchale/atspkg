module Language.ATS.Package.Build.C ( clibSetup
                                    , cpkgHome
                                    , allSubdirs
                                    ) where

import           Development.Shake.ATS
import           Development.Shake.C
import           Quaalude

cpkgHome :: CCompiler -> IO FilePath
cpkgHome cc' = (++ ("/.atspkg/" ++ ccToDir cc')) <$> getEnv "HOME"

allSubdirs :: FilePath -> IO [FilePath]
allSubdirs [] = pure mempty
allSubdirs d = do
    d' <- listDirectory d
    let d'' = ((d <> "/") <>) <$> d'
    ds <- filterM doesDirectoryExist d''
    ds' <- mapM allSubdirs ds
    pure $ join (ds : ds')


ccForConfig :: CCompiler -> String
ccForConfig = g . ccToString
    where g "icc" = "cc"
          g x     = x

makeExecutable :: FilePath -> [FilePath] -> IO ()
makeExecutable file dirs = do
    p <- findFile dirs file
    fold (setFileMode <$> p <*> pure ownerModes)

clibSetup :: Verbosity -- ^ Shake verbosity level
          -> CCompiler -- ^ C compiler
          -> String -- ^ Library name
          -> FilePath -- ^ Filepath to unpack to
          -> IO ()
clibSetup v cc' lib' p = do

    -- TODO autogen.sh

    -- Find configure script and make it executable
    subdirs <- allSubdirs p
    configurePath <- findFile (p:subdirs) "configure"
    autogenPath <- findFile (p:subdirs) "autogen.sh"
    cmakeLists <- findFile (p:subdirs) "CMakeLists.txt"
    fold (setFileMode <$> configurePath <*> pure ownerModes)
    fold (setFileMode <$> autogenPath <*> pure ownerModes)
    makeExecutable "install-sh" (p:subdirs)
    makeExecutable "mkinstalldirs" (p:subdirs)
    makeExecutable "rellns-sh" (p:subdirs)
    makeExecutable "shtool" (p:subdirs)

    -- CC="gcc" CFLAGS="-shared-libgcc -O2" ./glibc-2.27/configure --prefix="$HOME/.atspkg"
    -- Set environment variables for configure script
    h <- cpkgHome cc'
    let procEnv = Just [("CC", ccForConfig cc'), ("CFLAGS" :: String, "-I" <> h <> "include -Wno-error -O2"), ("PATH", "/usr/bin:/bin")]

    biaxe [fold (autogen v <$> autogenPath), fold (configure v h <$> configurePath <*> pure procEnv), cmake v h cmakeLists, make v, install v] lib' p

cmake :: Verbosity -> FilePath -> Maybe FilePath -> String -> FilePath -> IO ()
cmake _ _ Nothing _ _ = mempty
cmake v prefixPath (Just cfgLists) _ _ = do
    let p = takeDirectory cfgLists
    silentCreateProcess v ((proc "cmake" ["-DCMAKE_INSTALL_PREFIX:PATH=" ++ prefixPath, p]) { cwd = Just p })

autogen :: Verbosity -> FilePath -> String -> FilePath -> IO ()
autogen v autogenPath lib' _ =
    putStrLn ("generating " ++ lib' ++ "...") >>
    silentCreateProcess v ((proc autogenPath mempty) { cwd = Just (takeDirectory autogenPath) })

configure :: Verbosity -> FilePath -> FilePath -> Maybe [(String, String)] -> String -> FilePath -> IO ()
configure v prefixPath configurePath procEnv lib' p =
    putStrLn ("configuring " ++ lib' ++ "...") >>
    silentCreateProcess v ((proc configurePath ["--prefix", prefixPath, "--host", host]) { cwd = Just p, env = procEnv })

findMakefile :: FilePath -> IO FilePath
findMakefile p = do
    subdirs <- allSubdirs p
    -- mc <- findFile (p:subdirs) "CMakeLists.txt"
    mp <- findFile (p:subdirs) "Makefile"
    pure $ maybe p takeDirectory mp -- (maybe p takeDirectory mp) takeDirectory mc

make :: Verbosity -> String -> FilePath -> IO ()
make v lib' p = do
    putStrLn ("building " ++ lib' ++ "...")
    p' <- findMakefile p
    silentCreateProcess v ((proc "make" ["-j4"]) { cwd = Just p' })

install :: Verbosity -> String -> FilePath -> IO ()
install v lib' p = do
    putStrLn ("installing " ++ lib' ++ "...")
    p' <- findMakefile p
    silentCreateProcess v ((proc "make" ["install"]) { cwd = Just p' })
