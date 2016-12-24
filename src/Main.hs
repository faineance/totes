{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
import qualified Data.Map                         as Map
import           Data.Typeable                    (Typeable)
import           GHC.Generics
import           Unbound.Generics.LocallyNameless

data Expr = Data
          | Codata
          | Var (Name Expr)
          | Lambda (Bind (Name Expr) Expr)
          | Pi (Bind (Name Expr) Expr) -- "forall"
          | App Expr Expr
        deriving (Show, Generic, Typeable)

instance Alpha Expr


instance Subst Expr Expr where
    isvar (Var x) = Just ( SubstName x)
    isvar _       = Nothing


main :: IO ()
main = putStrLn "hello world"
