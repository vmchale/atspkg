import           Distribution.CommandLine
import           Distribution.Simple
import           Distribution.Types.HookedBuildInfo

main :: IO ()
main = defaultMainWithHooks $
    simpleUserHooks { preConf = \_ _ -> writeBashCompletions "atspkg" >> pure emptyHookedBuildInfo }
