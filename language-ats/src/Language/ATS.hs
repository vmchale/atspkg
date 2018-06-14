{-# LANGUAGE OverloadedStrings #-}

-- | Main module for the library
module Language.ATS ( -- * Functions for working with syntax
                      lexATS
                    , parse
                    , parseWithCtx
                    , parseM
                    , printATS
                    , printATSCustom
                    , printATSFast
                    , printErr
                    , warnErr
                    , defaultFixityState
                    -- * Library functions
                    , getDependencies
                    -- * Syntax Tree
                    , ATS (..)
                    , Declaration (..)
                    , Expression (..)
                    , Type (..)
                    , Function (..)
                    , Implementation (..)
                    , Pattern (..)
                    , Name (..)
                    , UnOp (..)
                    , BinOp (..)
                    , DataPropLeaf (..)
                    , Leaf (..)
                    , DataSortLeaf (..)
                    , Arg (..)
                    , Addendum (..)
                    , LambdaType (..)
                    , Universal (..)
                    , Existential (..)
                    , PreFunction (..)
                    , StaticExpression (..)
                    , StackFunction (..)
                    , Paired (..)
                    , Fixity (..)
                    , SortArg (..)
                    , Sort (..)
                    , SortArgs
                    , Fix
                    -- * Parser State
                    , FixityState
                    -- * Lexical types
                    , Token (..)
                    , AlexPosn (..)
                    , Keyword (..)
                    -- * Error types
                    , ATSError (..)
                    -- * Lenses
                    , preF
                    , expression
                    , fun
                    , leaves
                    , constructorUniversals
                    , typeCall
                    , typeCallArgs
                    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           GHC.IO.Handle.FD             (stderr)
import           Language.ATS.Lexer
import           Language.ATS.Parser
import           Language.ATS.PrettyPrint
import           Language.ATS.Types
import           Lens.Micro
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

rewriteATS' :: Eq a => (ATS a, FixityState a) -> ATS a
rewriteATS' (ATS ds, st) = ATS (rewriteDecl st <$> ds)

-- | Print an error message to standard error.
printErr :: MonadIO m => ATSError -> m ()
printErr = liftIO . hPutDoc stderr . (<> "\n") . pretty

-- | Same as 'printErr', but print a yellow warning message instead.
warnErr :: MonadIO m => FilePath -> ATSError -> m ()
warnErr fp = liftIO . hPutDoc stderr . ((dullyellow "Warning" <+> text (fp <> ":")) <+> ) . preErr
-- TODO: this should detect if being piped to terminal!

-- | Parse a string containing ATS source, disregarding comments.
parseM :: String -> Either ATSError (ATS AlexPosn)
parseM = parseWithCtx defaultFixityState stripComments

-- | Parse a string containing ATS source.
parse :: String -> Either ATSError (ATS AlexPosn)
parse = parseWithCtx defaultFixityState id

lexErr :: Either String a -> Either ATSError a
lexErr = over _Left LexError

stripComments :: [Token] -> [Token]
stripComments = filter nc
    where nc CommentLex{}      = False
          nc CommentBegin{}    = False
          nc CommentEnd{}      = False
          nc CommentContents{} = False
          nc _                 = True

-- | Parse with some fixity declarations already in scope.
parseWithCtx :: FixityState AlexPosn -> ([Token] -> [Token]) -> String -> Either ATSError (ATS AlexPosn)
parseWithCtx st p = stateParse <=< lex'
    where withSt = flip runStateT st
          lex' = lexErr . fmap p . lexATS
          stateParse = fmap rewriteATS' . withSt . parseATS

-- | Extract a list of files that some code depends on.
getDependencies :: ATS a -> [FilePath]
getDependencies (ATS ds) = g =<< ds
    where g (Load _ _ _ s)   = [s]
          g (Include s)      = [s]
          g (Local _ as as') = foldMap getDependencies [as, as']
          g _                = mempty
