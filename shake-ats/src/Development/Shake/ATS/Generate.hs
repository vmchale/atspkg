module Development.Shake.ATS.Generate ( generateLinks
                                      ) where

import           Control.Lens
import           Language.ATS

generateLinks :: String -> Either (ATSError String) String
generateLinks = fmap (printATS . generateLinks') . parse

generateLinks' :: ATS -> ATS
generateLinks' (ATS ds) = ATS (macDecl : fmap g ds)
    where g f@Func{} = Extern undefined (set (fun.preF.expression) expr f)
          g x        = x
          expr = Just (StringLit "\"mac#\"")
          macDecl = Define "ATS_MAINATSFLAG 1"
