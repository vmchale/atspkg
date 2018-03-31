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

clibSetup :: CCompiler -- ^ C compiler
          -> String -- ^ Library name
          -> FilePath -- ^ Filepath to unpack to
          -> IO ()
clibSetup cc' lib' p = do

    -- Find configure script and make it executable
    subdirs <- allSubdirs p
    configurePath <- findFile (p:subdirs) "configure"
    cmakeLists <- findFile (p:subdirs) "CMakeLists.txt"
    fold (setFileMode <$> configurePath <*> pure ownerModes)

    -- Set environment variables for configure script
    h <- cpkgHome cc'
    let procEnv = Just [("CC", ccToString cc'), ("CFLAGS" :: String, "-I" <> h <> "include"), ("PATH", "/usr/bin:/bin")]

    biaxe [fold (configure h <$> configurePath <*> pure procEnv), cmake h cmakeLists, make, install] lib' p

-- TODO only do this if @./configure@ is missing
cmake :: FilePath -> Maybe FilePath -> String -> FilePath -> IO ()
cmake _ Nothing _ _ = mempty
cmake prefixPath (Just cfgLists) _ _ = do
    let p = takeDirectory cfgLists
    silentCreateProcess ((proc "cmake" ["-DCMAKE_INSTALL_PREFIX:PATH=" ++ prefixPath, p]) { cwd = Just p })

configure :: FilePath -> FilePath -> Maybe [(String, String)] -> String -> FilePath -> IO ()
configure prefixPath configurePath procEnv lib' p =
    putStrLn ("configuring " ++ lib' ++ "...") >>
    silentCreateProcess ((proc configurePath ["--prefix", prefixPath, "--host", host]) { cwd = Just p, env = procEnv })

{-findMakefile :: FilePath -> IO FilePath
findMakefile p = do
    subdirs <- allSubdirs p
    mc <- findFile (p:subdirs) "configure"
    mp <- findFile (p:subdirs) "Makefile"
    pure $ maybe (maybe p takeDirectory mp) takeDirectory mc-}

make :: String -> FilePath -> IO ()
make lib' p = do
    putStrLn ("building " ++ lib' ++ "...")
    -- p' <- findMakefile p
    silentCreateProcess ((proc "make" ["-j4"]) { cwd = Just p })

install :: String -> FilePath -> IO ()
install lib' p = do
    putStrLn ("installing " ++ lib' ++ "...")
    -- p' <- findMakefile p
    silentCreateProcess ((proc "make" ["install"]) { cwd = Just p })
