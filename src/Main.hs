module Main where
import           Control.Monad.Trans
import           System.Console.Repline

import           Data.List              (isPrefixOf)
import           System.Exit
import           Term
import Parser
type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input


completer :: Monad m => WordCompleter m
completer n = do
  let names = [":load", ":type"]
  return $ filter (isPrefixOf n) names

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
    ("load", help)
  , ("t", say)
  , ("q", quit)
  ]

ini :: Repl ()
ini = liftIO $ putStrLn ""

repl :: IO ()
repl = evalRepl "> " cmd options (Word0 completer) ini

main :: IO ()
main = repl
