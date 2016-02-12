module MSMT.Util.Cli
  ( showHelpText
  , showHelp
  ) where


import           Control.Monad
import           Control.Monad.IO.Class
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.Exit
import           System.Process


showHelpText :: ParserInfo a -> IO ()
showHelpText info = do
  prog <- getProgName
  let (msg, exit) = renderFailure failure prog
  case exit of
    ExitSuccess -> putStrLn msg
    _           -> die "Could not load helptext. This never should happen!"
  where
    failure = parserFailure defaultPrefs info ShowHelpText mempty

showHelp :: ParserInfo a -> [String] -> IO ()
showHelp parser help = do
  showHelpText parser
  putStr $ unlines help
  exitSuccess
