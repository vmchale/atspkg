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

    biaxe [fold (configure h <$> configurePath <*> pure procEnv), cmake cmakeLists, make, install] lib' p

cmake :: Maybe FilePath -> String -> FilePath -> IO ()
cmake Nothing _ _ = mempty
cmake (Just cfgLists) _ _ = do
    let p = takeDirectory cfgLists
    silentCreateProcess ((proc "cmake" [p]) { cwd = Just p })

configure :: FilePath -> FilePath -> Maybe [(String, String)] -> String -> FilePath -> IO ()
configure prefixPath configurePath procEnv lib' p =
    putStrLn ("configuring " ++ lib' ++ "...") >>
    silentCreateProcess ((proc configurePath ["--prefix", prefixPath, "--host", host]) { cwd = Just p, env = procEnv })

make :: String -> FilePath -> IO ()
make lib' p =
    putStrLn ("building " ++ lib' ++ "...") >>
    silentCreateProcess ((proc "make" ["-j4"]) { cwd = Just p })

install :: String -> FilePath -> IO ()
install lib' p =
    putStrLn ("installing " ++ lib' ++ "...") >>
    silentCreateProcess ((proc "make" ["install"]) { cwd = Just p })
