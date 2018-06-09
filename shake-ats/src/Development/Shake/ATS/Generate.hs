module Development.Shake.ATS.Generate ( generateLinks
                                      ) where

import           Language.ATS
import           Lens.Micro

generateLinks :: String -> Either ATSError String
generateLinks = fmap (printATS . generateLinks') . parseM

generateLinks' :: ATS a -> ATS a
generateLinks' (ATS ds) = ATS (fmap g ds ++ [macDecl])
    where g f@Func{} = Extern undefined (set (fun.preF.expression) expr f)
          g x        = x
          expr = Just (StringLit "\"mac#\"")
          macDecl = Define "#define ATS_MAINATSFLAG 1"
