import           Distribution.CommandLine
import           Distribution.Simple

main :: IO ()
main = setManpath >>
    writeManpages "man/atsfmt.1" "atsfmt.1" >>
    writeBashCompletions "atsfmt" >>
    defaultMain
