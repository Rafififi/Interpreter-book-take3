module Main (main) where
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStrLn)
import System.Environment (getArgs)
import Control.Monad.IO.Class (MonadIO(liftIO))
import CombinatorBase (GenParser(runParser))
import Scanner(tokens)

type Repl a = InputT IO a

runner :: [String] -> Repl ()
runner [file] = liftIO $ runFile file 
runner []     = runPrompt 
runner _      = liftIO $ print "INCORRECT USAGE: hisp [SOURCE]"


runPrompt :: Repl ()
runPrompt = do
  minput <- getInputLine "> "
  case minput of 
    Nothing     -> outputStrLn ""
    Just []     -> outputStrLn ""
    Just "\EOT" -> outputStrLn ""
    Just ":q"   -> outputStrLn ""
    Just input  -> liftIO (run input >>= printFail) >> runPrompt
      where printFail True =  print "failed"
            printFail False = print ""

runFile :: String -> IO ()
runFile file = readFile file >>= run >>= hadError
  where 
    hadError True  = exitWith (ExitFailure 65)
    hadError False = return ()

run :: String -> IO Bool
run source = case runParser tokens source (0,1) of
                Left err  -> print err >> return True
                Right res -> print res >> return False


main :: IO ()
main = do
  args <- getArgs
  runInputT defaultSettings $ runner args
