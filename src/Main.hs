module Main where
import           Control.Monad.Trans
import qualified Data.Map                         as Map
import           Data.Typeable                    (Typeable)
import           GHC.Generics
import           System.Console.Haskeline
import           Term
import           Unbound.Generics.LocallyNameless

type Repl a = InputT IO a

settings :: Settings IO
settings = defaultSettings { historyFile  = Just "totes" }

process :: String -> IO ()
process = putStrLn

repl :: Repl ()
repl = do
    minput <- getInputLine "> "
    case minput of
        Nothing    -> outputStrLn "Exiting"
        Just input -> liftIO (process input) >> repl

main :: IO ()
main = runInputT settings repl
