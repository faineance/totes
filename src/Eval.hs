module Eval where
import           Control.Monad.IO.Class
import           Term
import           Unbound.Generics.LocallyNameless
eval :: Fresh m => Term -> m Term
eval (Var x) = return $ Var x
eval (Lambda binding) = do (x, e) <- unbind binding
                           e' <- eval e
                           return (Lambda (bind x e'))
eval (Let binding) =
    do (f, e) <- unbind binding
       let s = subst f (Let binding) e in
         eval s
eval (App t1 t2) =
    do  e1 <- eval t1
        e2 <- eval t2
        case e1 of
            Lambda binding -> do
                (x, e1') <- unbind binding
                let s = subst x e2 e1' in
                    eval s
            _ -> return $ App e1 e2
