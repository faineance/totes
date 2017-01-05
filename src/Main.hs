{-# OPTIONS_GHC -Wall #-}
module Main where

import           Control.Monad.State.Strict
import           Control.Monad.Trans
import           Data.List                        (isPrefixOf)
import           Data.Map                         (singleton)
import           Debug.Trace
import           Eval
import           Parser
import           PrettyPrint
import           System.Console.Repline
import           System.Environment
import           System.Exit
import           Term
import           Typecheck
import           Unbound.Generics.LocallyNameless
type Repl a = HaskelineT (StateT Env IO) a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = exec input

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load" , fileCompleter)
  , (":l"    , fileCompleter)
  ]

completer :: Monad m => WordCompleter m
completer n = do
  let names = [":load", ":type", ":q", ":aeq"]
  return $ filter (isPrefixOf n) names

load :: [String] -> Repl ()
load args = do
    contents <- liftIO $ readFile (unwords args)
    trace contents (return ())


-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

say :: [String] -> Repl ()
say args = do
    _ <- liftIO $ print (unwords args)
    return ()


type' :: [String] -> Repl ()
type' args = do
    _ <- liftIO $ putStrLn (either show (pp . fst) ty)
    return ()
    where
        ty = infer empty (parseTerm (unwords args))
hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
      liftIO $ print err
      abort

exec :: String -> Repl ()
exec source = do
    st <- get
    ty <- hoistErr $ infer st parsed
    _ <- liftIO $ putStrLn (pp parsed ++ " :: " ++ pp (fst ty) )
    put st
        where
            parsed = parseTerm source

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-- alphaEquiv :: [String] -> Repl ()
--
-- alphaEquiv args = do
--     _ <- liftIO $ putStrLn (show (parseTerm t1 `aeq` (parseTerm t2)))
--     return ()

options :: [(String, [String] -> Repl ())]
options = [
    ("l", load)
  , ("load", load)
  , ("t", type')
  , ("type", type')
  , ("q", quit)
  , ("quit", quit)
  -- , ("aeq", alphaEquiv) -- alpha equiv
  , ("beq", say) -- beta equiv
  ]

ini :: Repl ()
ini = liftIO $ putStrLn ""

repl :: Repl a -> IO ()
repl pre = evalStateT (evalRepl "> " cmd options (Prefix (wordCompleter completer) defaultMatcher) ini) empty
-- (singleton (Var (bind (s2n "id")))  (Lambda (bind (s2n "x") (Var  (s2n "x")))))
main :: IO ()
main = do
    args <- getArgs
    case args of
        []         -> repl (return ())
        [fileName] -> repl (load [fileName])
        _          -> putStrLn "invalid arguments"
