{-# LANGUAGE CPP #-}

module Quaalude ( bool
                , intersperse
                , void
                , unless
                , when
                , join
                , zipWithM_
                , zipWithM
                , filterM
                , encode
                , decode
                , fromMaybe
                , isPrefixOf
                , isSuffixOf
                , on
                , (<>)
                , (***)
                , (&&&)
                , (<=<)
                , first
                , second
                , getEnv
                , MonadIO (..)
                -- * "Data.Text.Lazy" reëxports
                , Text
                , pack
                , unpack
                , (.*)
                -- * Dhall reëxports
                , Interpret
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
                , red
                , hang
                , putDoc
                , Pretty (pretty)
                , module X
                ) where

import           Control.Arrow                hiding ((<+>))
import           Control.Lens                 as X
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy         (ByteString)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Posix.Files
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Control.Composition
import           Control.Monad
import           Data.Binary
import           Data.Bool                    (bool)
import           Data.List                    (intersperse, isPrefixOf, isSuffixOf)
import           Data.Maybe                   (fromMaybe)
import           Data.Text.Lazy               (pack, unpack)
import           Development.Shake            hiding (getEnv)
import           Development.Shake.FilePath
import           Dhall                        hiding (bool)
import           System.Directory             as X
import           System.Environment           (getEnv)
import           Text.PrettyPrint.ANSI.Leijen hiding (bool, (<>))

infixr 5 <#>

-- | Same as "Text.PrettyPrint.ANSI.Leijen"'s @<$>@, but doesn't clash with the
-- prelude.
(<#>) :: Doc -> Doc -> Doc
(<#>) a b = a <> line <> b
