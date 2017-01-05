{-# LANGUAGE ViewPatterns #-}
module Typecheck where
import           Term

import           Control.Monad.Except
import           Control.Monad.Trans

import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import           Unbound.Generics.LocallyNameless

import           Control.Applicative
import           Control.Monad.Reader

data TypeError
  = BadApp { expected :: Type, received :: Type }
  | NotAFn { got :: Type }
  deriving (Show)

type Env = (Map.Map TermName Type, Set.Set Type)
type TypeM = ExceptT TypeError FreshM
typeof :: Env -> Term -> TypeM Type
typeof env t = case t of
  Var n -> case Map.lookup n (fst env) of
          Nothing -> undefined
          Just t  -> return $ TVar ((s2n n))
  -- Lambda bnd -> do
  --   ((n , unembed -> ty), body) <- unbind bnd
  --   rgt <- local (Map.insert n ty) $ typeof body
  --   return $ TArr ty rgt
  -- LAM bnd -> do
  --   (n, bod) <- undbind bnd
  --   inner <- typeof bod
    -- return $ TyLam (bind n inner)
  -- App e1 e2 -> do
  --   t1 <- typeof e1
  --   t2 <- typeof e2
  --   case t1 of
  --     TyArr t11 t12 -> do
  --       if t11 == t2
  --         then return t12
  --         else throwError $ BadApp t11 t2
  --     _ -> throwError $ NotAFn t1
  -- TyApp e t2 -> do
  --   t1 <- typeof e
  --   case t1 of
  --     TyLam bnd -> do
  --       (tyx, tbod) <- unbind bnd
  --       return $ subst x t2 tbod
