{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Development.Shake.TH ( checkExecutable
                            , mkVersions
                            , mkExecChecks
                            , commonVersion
                            , MBool
                            ) where

import           Control.Monad.IO.Class
import           Data.Maybe             (isJust)
import           Development.Shake
import           Language.Haskell.TH
import           System.Directory       (findExecutable)

type MBool = forall m. MonadIO m => m Bool

-- | Attempt to get version information from a given exectuable.
commonVersion :: String -- ^ Executable name
              -> Action String
commonVersion prog = do
    ~(Stdout out) <- command mempty prog ["--version"]
    pure . last . words . head . lines $ out

mkSigVersion :: String -> Dec
mkSigVersion s = SigD (mkName $ s ++ "Version") (ConT ''Action `AppT` ConT ''String)

mkVersion :: String -> Dec
mkVersion s = FunD (mkName $ s ++ "Version") [Clause mempty (NormalB expr) mempty]
    where expr = VarE 'commonVersion `AppE` (LitE $ StringL s)

mkVersions :: [String] -> Q [Dec]
mkVersions = pure . (g =<<)
    where g s = [mkVersion s, mkSigVersion s]

mkSig :: String -> Dec
mkSig s = SigD (mkName s) (ConT ''MBool)

-- | Check for the presence of some executable.
checkExecutable :: (MonadIO m) => String -> m Bool
checkExecutable = fmap isJust . liftIO . findExecutable

mkExecCheck :: String -> Dec
mkExecCheck s = FunD (mkName s) [Clause mempty (NormalB expr) mempty]
    where expr = VarE 'checkExecutable `AppE` (LitE $ StringL s)

mkExecChecks :: [String] -> Q [Dec]
mkExecChecks = pure . (=<<) g
    where g s = [mkExecCheck s, mkSig s]
