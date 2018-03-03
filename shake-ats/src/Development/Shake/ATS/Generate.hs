module Development.Shake.ATS.Generate ( generateLinks
                                      ) where

import           Control.Lens
import           Data.Semigroup
import           Language.ATS

generateLinks :: String -> Either ATSError String
generateLinks = fmap (printATS . generateLinks') . parseM

generateLinks' :: ATS a -> ATS a
generateLinks' (ATS ds) = ATS (fmap g ds <> [macDecl]) --  ATS [Local undefined (ATS (fmap g ds <> [macDecl])) (ATS mempty)]
    where g f@Func{} = Extern undefined (set (fun.preF.expression) expr f)
          g x        = x
          expr = Just (StringLit "\"mac#\"")
          macDecl = Define "#define ATS_MAINATSFLAG 1"
