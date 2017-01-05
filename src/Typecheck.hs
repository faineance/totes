{-# LANGUAGE ViewPatterns #-}
module Typecheck where
import           Control.Monad.Except
import           Control.Monad.Trans
import           Debug.Trace
import           Term

import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import           Unbound.Generics.LocallyNameless

import           Control.Applicative
import           Control.Monad.Reader


type Env = Map.Map (Name Term) Type
type Constraint = (Type, Type)

data TypeError
    = BadApp { expected :: Type, received :: Type }
    | NotAFn { got :: Type }
    | UnboundVariable (Name Term)
    deriving (Show)

empty :: Env
empty = Map.empty

freshtv :: InferM Type
freshtv = do
  x <- fresh (string2Name "T")
  return $ TVar x



type InferM = ExceptT TypeError FreshM
type TypeM = ExceptT TypeError LFreshM



infer' :: Env -> Term -> InferM (Type, [Constraint])
infer' env term | trace ("infer was called: "++show term) False = undefined
infer' env term = case term of
    Lambda b -> do
        (n, e) <- unbind b
        tv <- freshtv
        let env' = Map.insert n tv env
        traceM (show env')
        (t, cs) <- infer' env' e
        return (TArr tv t, cs)
    App e1 t1 e2 -> do
        (t1, cs1) <- infer' env e1
        (t2, cs2) <- infer' env e2
        tv <- freshtv
        return (tv, (t1, TArr t2 tv) : cs1 ++ cs2)
    Var n -> case Map.lookup n env of
        Nothing -> throwError $ UnboundVariable n
        Just t  -> return (t, [])
    Data -> return (TData,[])



typeof' :: Env -> Term -> Type -> TypeM Bool
typeof' env (Var n) ty= case Map.lookup n env of
    Nothing  -> throwError $ UnboundVariable n
    Just ty' -> return (ty `aeq` ty')
typeof' env (Lambda bnd) (TArr t1 t2) =
          lunbind bnd (\(x , e) ->
            typeof' (Map.insert x t1 env) e t2)
typeof' env (App e1 t1 e2) t2 = do
      b1 <- typeof' env e1 (TArr t1 t2)
      b2 <- typeof' env e2 t1
      return $ b1 && b2
typeof' _ _ _ = return False

typeof :: Env -> Term -> Type ->  Either TypeError Bool
typeof env term ty = runLFreshM (runExceptT (typeof' env term ty))

infer :: Env -> Term -> Either TypeError (Type, [Constraint])
infer env term = runFreshM (runExceptT (infer' env term))
