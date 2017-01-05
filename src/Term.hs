{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Term where

import           Control.Monad.Except
import           Control.Monad.Trans
import qualified Data.Map                         as Map

import           Data.Typeable                    (Typeable)
import           Debug.Trace
import           GHC.Generics
import           Unbound.Generics.LocallyNameless


data Type = TVar (Name Type)
          | TArr Type Type
          | TData
        deriving (Show, Generic, Typeable)

-- data Base = Data  -- type of types ( finite )
--            | Codata  -- type of types ( infinite (corecursive) )
--         deriving (Show, Generic, Typeable)

data Term = Var !(Name Term)
          | Lambda !(Bind (Name Term) Term)
        --   | Pi !(Bind (Name Term) Term) -- "forall"
          | App !Term Type !Term
          | Data
        deriving (Show, Generic, Typeable)

newtype Definition = Definition (Bind (Name Term) Term)
        deriving (Show, Generic, Typeable)

newtype Module = Module [Definition]

instance Alpha Type
instance Alpha Term

instance Subst Type Type where
  isvar (TVar v) = Just (SubstName v)
  isvar _        = Nothing

instance Subst Term Term where
    isvar (Var x) = Just ( SubstName x)
    isvar _       = Nothing

instance Subst Term Type where
