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
        deriving (Show, Generic, Typeable)

data Term = Data -- type of types ( finite )
          | Codata -- type of types ( infinite (corecursive) )
          | Var !(Name Term)
          | Lambda !(Bind (Name Term) Term)
        --   | Pi !(Bind (Name Term) Term) -- "forall"
          | App !Term !Term
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

data TypeError
  = UnboundVariable (Name Term)
  | GenericTypeError
  deriving (Show)

type Infer = ExceptT TypeError FreshM


type Env = Map.Map (Name Term) Type
type Constraint = (Type, Type)

empty :: Env
empty = Map.empty

freshtv :: Infer Type
freshtv = do
  x <- fresh (string2Name "t")
  return $ TVar x

infer' :: Env -> Term -> Infer (Type, [Constraint])
infer' env term | trace ("infer was called: "++show term) False = undefined
infer' env term = case term of
    Lambda b -> do
        (n, e) <- unbind b
        tv <- freshtv
        let env' = Map.insert n tv env
        (t, cs) <- infer' env' e
        return (TArr tv t, cs)
    App e1 e2 -> do
        (t1, cs1) <- infer' env e1
        (t2, cs2) <- infer' env e2
        tv <- freshtv
        return (tv, (t1, TArr t2 tv) : cs1 ++ cs2)
    Var n -> case Map.lookup n env of
        Nothing -> throwError $ UnboundVariable n
        Just t  -> return (t, [])

infer :: Env -> Term -> Either TypeError (Type, [Constraint])
infer env term = runFreshM (runExceptT (infer' env term))
