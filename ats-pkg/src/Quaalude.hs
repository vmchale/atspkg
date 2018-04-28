{-# OPTIONS_GHC -fno-warn-orphans #-}

module Quaalude ( hex
                , bool
                , intersperse
                , transpose
                , sortBy
                , void
                , unless
                , when
                , join
                , fold
                , zipWithM_
                , zipWithM
                , filterM
                , encode
                , decode
                , fromMaybe
                , isPrefixOf
                , isSuffixOf
                , on
                , both
                , (***)
                , (&&&)
                , (<=<)
                , (<>)
                , first
                , second
                , getEnv
                , exitWith
                , showVersion
                , ExitCode (ExitSuccess)
                , MonadIO (..)
                -- * "System.Process.Ext" reëxports
                , silentCreateProcess
                -- * "Data.Text.Lazy" reëxports
                , Text
                , pack
                , unpack
                -- * "Control.Composition" reëxports
                , biaxe
                , (.*)
                , (.**)
                , thread
                -- * Dhall reëxports
                , Interpret
                , Inject
                , Generic
                , Binary
                , input
                , auto
                , detailed
                -- * Shake reëxports
                , Rules
                , Action
                , command
                , command_
                , (%>)
                , need
                , want
                , shake
                , Rebuild (..)
                , (~>)
                , cmd
                , cmd_
                , ShakeOptions (..)
                , shakeOptions
                , copyFile'
                , Change (..)
                , Verbosity (..)
                , removeFilesAfter
                , Lint (..)
                , takeBaseName
                , takeFileName
                , takeDirectory
                , (-<.>)
                -- * "System.Posix" reëxports
                , setFileMode
                , ownerModes
                -- * "Network.HTTP.Client.TLS" reëxports
                , tlsManagerSettings
                -- "Network.HTTP.Client" reëxports
                , newManager
                , parseRequest
                , httpLbs
                , Response (..)
                , Request (method, redirectCount)
                -- * ByteString reëxports
                , ByteString
                -- * Helpers for pretty-printing
                , (<#>)
                -- * "Text.PrettyPrint.ANSI.Leijen" reëxports
                , (<+>)
                , text
                , punctuate
                , dullred
                , linebreak
                , dullyellow
                , hardline
                , hang
                , indent
                , putDoc
                , Pretty (pretty)
                , module X
                -- Lens exports
                , over
                , _Just
                , view
                , _1
                , _2
                , _4
                , makeLensesFor
                , makeLenses
                , each
                , (&)
                , (%~)
                ) where

import           Control.Arrow                hiding ((<+>))
import           Control.Composition
import           Control.Lens                 hiding (both)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary
import           Data.Bool                    (bool)
import           Data.ByteString.Lazy         (ByteString)
import           Data.Foldable
import           Data.List
import           Data.Maybe                   (fromMaybe)
import           Data.Semigroup
import           Data.Text.Lazy               (pack, unpack)
import           Data.Version                 (showVersion)
import           Development.Shake            hiding (getEnv)
import           Development.Shake.FilePath
import           Dhall                        hiding (bool)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Numeric                      (showHex)
import           System.Directory             as X
import           System.Environment           (getEnv)
import           System.Exit                  (ExitCode (ExitSuccess), exitWith)
import           System.Posix.Files
import           System.Process               as X
import           System.Process.Ext
import           Text.PrettyPrint.ANSI.Leijen hiding (bool, (<$>), (<>))

infixr 5 <#>

hex :: Int -> String
hex = flip showHex mempty

instance Semigroup a => Semigroup (Action a) where
    (<>) a b = (<>) <$> a <*> b

instance (Semigroup a, Monoid a) => Monoid (Action a) where
    mempty = pure mempty
    mappend = (<>)

-- | Same as "Text.PrettyPrint.ANSI.Leijen"'s @<$>@, but doesn't clash with the
-- prelude.
(<#>) :: Doc -> Doc -> Doc
(<#>) a b = a <> line <> b
