module Eval where
import           Control.Monad.IO.Class
import           Term
import           Unbound.Generics.LocallyNameless

eval :: Fresh m => Term -> m Term
eval (Var x) = return $ Var x
eval (Lambda binding) = do (x, e) <- unbind binding
                           e' <- eval e
                           return (Lambda (bind x e'))
eval (App t1 ty1 t2) = do
        e2 <- eval t2
        e1 <- eval t1
        case e1 of
            Lambda binding -> do
                (x, e1') <- unbind binding
                let s = subst x e2 e1' in
                    eval s
            _ -> return $ App e1 ty1 e2
