module Main where

import           Control.Monad.Trans
import           Data.List              (isPrefixOf)
import           Data.Version
import           Debug.Trace
import           Parser
import           Paths_totes            (version)
import           System.Console.Repline
import           System.Environment
import           System.Exit
import           Term
type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input


completer :: Monad m => WordCompleter m
completer n = do
  let names = [":load", ":type"]
  return $ filter (isPrefixOf n) names

load :: [String] -> Repl ()
load args = do
    contents <- liftIO $ readFile (unwords args)
    trace contents (return ())
    --mapM (exec True) (L.lines contents)


-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

say :: [String] -> Repl ()
say args = do
    _ <- liftIO $ print (unwords args)
    return ()

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

options :: [(String, [String] -> Repl ())]
options = [
    ("load", load)
  , ("l", load)
  , ("t", say)
  , ("type", say)
  , ("q", quit)
  , ("aeq", say) -- alpha equiv
  , ("beq", say) -- beta equive
  ]

ini :: Repl ()
ini = liftIO $ putStrLn ""

repl :: Repl a ->IO ()
repl pre = evalRepl "> " cmd options (Word0 completer) ini

main :: IO ()
main = do
    args <- getArgs
    case args of
        []         -> repl (return ())
        [fileName] -> repl (load [fileName])
