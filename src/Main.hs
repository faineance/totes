{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
import           Control.Monad.Trans
import qualified Data.Map                         as Map
import           Data.Typeable                    (Typeable)
import           GHC.Generics
import           System.Console.Haskeline
import           Unbound.Generics.LocallyNameless


data Term = Data -- the type of types ( finite )
          | Codata -- the type of types ( infinite (corecursive) )
          | Var (Name Term)
          | Lambda (Bind (Name Term) Term)
          | Pi (Bind (Name Term) Term) -- "forall"
          | App Term Term
        deriving (Show, Generic, Typeable)

instance Alpha Term


instance Subst Term Term where
    isvar (Var x) = Just ( SubstName x)
    isvar _       = Nothing




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
