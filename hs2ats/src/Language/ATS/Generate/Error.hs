{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.ATS.Generate.Error ( -- * Types
                                     GenerateError (..)
                                   , ErrM
                                   -- * Functions
                                   , displayErr
                                   -- * Helper functions
                                   , unsupported
                                   , syntaxError
                                   , malformed
                                   ) where

import           Control.DeepSeq              (NFData)
import           GHC.Generics                 (Generic)
import           Language.Haskell.Exts        hiding (Pretty, loc)
import           System.IO                    (stderr)
import           Text.PrettyPrint.ANSI.Leijen

deriving instance NFData SrcLoc

displayErr :: GenerateError -> IO ()
displayErr = hPutDoc stderr . pretty

type ErrM a = Either GenerateError a

syntaxError :: SrcLoc -> String -> ErrM a
syntaxError = (Left .) . HaskellSyntaxError

unsupported :: String -> ErrM a
unsupported = Left . Unsupported

malformed :: String -> ErrM a
malformed = Left . Malformed

data GenerateError = Unsupported String
                   | HaskellSyntaxError SrcLoc String
                   | Internal String
                   | Malformed String
                   deriving (Eq, Show, Generic, NFData)

instance Pretty GenerateError where
    pretty (Unsupported s)            = dullyellow "Warning:" <+> "skipping unsupported construct" <$$> indent 2 (squotes (text s)) <> linebreak
    pretty (HaskellSyntaxError loc s) = dullred "Error:" <+> "failed to parse" <+> text (show loc) <> colon <$$> indent 2 (text s) <> linebreak
    pretty (Internal s)               = dullred "Error:" <+> "internal error: " <$$> indent 2 (text s) <> linebreak
    pretty (Malformed s)              = dullred "Error:" <+> "incompatible type" <$$> indent 2 (squotes (text s)) <> linebreak
