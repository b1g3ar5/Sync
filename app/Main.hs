
import System.IO
import System.Environment
import System.Exit
 
import Sync


main :: IO [[String]]
main = getArgs >>= parse 
 
parse::[[Char]]-> IO [[String]]
parse [] = usage   >> exit
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse ["brayfordlets"] = brayfordlets
parse ["bookclub"] = bookclub
parse _ = undefined
 
usage :: IO ()
usage   = putStrLn "Usage: Sync [brayfordlets|bookclub]"

version :: IO ()
version = putStrLn "Haskell Sync 0.1"

exit :: IO a
exit    = exitWith ExitSuccess

die :: IO a
die     = exitWith (ExitFailure 1)


