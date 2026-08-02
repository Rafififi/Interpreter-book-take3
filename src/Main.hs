module Main (main) where
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStrLn)
import System.Environment (getArgs)
import Control.Monad.IO.Class (MonadIO(liftIO))
import CombinatorBase (GenParser(runParser))
import Token(printTokens, TokenInfo(..), TokenType(UNEXPECTED))
import Scanner(tokens)
import Parser(parseTokens)
import Control.Monad (foldM)
import GHC.IO (unsafePerformIO)
import Interpreter (interpret)

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
    Just input  -> liftIO (run (printId input)>>= printFail) >> runPrompt
      where printFail ScannerFail     = print "Scanner failed"
            printFail ParserFail      = print "Parser failed"
            printFail InterpreterFail = print "Interpreter Failed"
            printFail Success         = print "Success"

runFile :: String -> IO ()
runFile file = readFile file >>= run >>= hadError
  where 
    hadError ScannerFail = exitWith (ExitFailure 65)
    hadError ParserFail =  exitWith (ExitFailure 66)
    hadError InterpreterFail =  exitWith (ExitFailure 67)
    hadError Success =  return ()

hasError :: [TokenInfo] -> Bool
hasError (TokenInfo _ _ (UNEXPECTED _):_) = True
hasError [] = False
hasError (_:xs) = hasError xs

printId :: Show a => a -> a
printId a = unsafePerformIO $ print a >> return a

data FailureCase = ScannerFail
                 | ParserFail
                 | InterpreterFail
                 | Success

run :: String -> IO FailureCase
run source = case runParser tokens source (0,1) of
                Left err  -> print "error" >> print err >> return ScannerFail
                Right (_, (toks, _)) -> case hasError toks of
                  True -> foldM printTokens False toks >> return ScannerFail
                  False -> case parseTokens (printId toks) [] of
                    Left err  -> print ("Error: " <> (show err)) >> return ParserFail
                    Right res -> print ("Result" <> show res) >> case interpret res of
                                  "" -> return Success
                                  xs -> print xs >> return InterpreterFail


main :: IO ()
main = do
  args <- getArgs
  runInputT defaultSettings $ runner args
