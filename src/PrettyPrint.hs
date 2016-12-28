{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PrettyPrint where
import           Debug.Trace
import           Term
import           Text.PrettyPrint.ANSI.Leijen     hiding (Pretty)
import           Unbound.Generics.LocallyNameless

farrow :: Doc
farrow = white (text "->")

class Pretty p where
    pp' :: LFresh m => p -> m Doc

instance Pretty (Name a) where
    pp' = return . name

instance Pretty Type where
    -- pp' p | trace ("pp was called: " ++ show p) False = undefined
    pp' (TVar n) = return $ name n
    pp' (TArr t1 t2) = do
          p1 <- pp' t1
          p2 <- pp' t2
          return $ text "\\" <> p1 <+> farrow <+> p2

instance Pretty Term where
    -- pp' p | trace ("pp was called: " ++ show p) False = undefined
    pp' Data    = return $ text "Data"
    pp' Codata  = return $ text "Codata"
    pp' (Var n) = return $ name n
    pp' (App e1 e2) = do
        p1 <- pp' e1
        p2 <- pp' e2
        return $ parens $ p1 <+> p2
    pp' (Lambda b) = lunbind b $ \(n, e) -> do
        pe <- pp' e
        return $ backslash <> name n <+> farrow <+> pe

instance Pretty Definition where
    pp' (Definition d) = lunbind d $ \(n, e) -> do
        pe <- pp' e
        return $ name n <+> white (char '=') <+> pe

instance Pretty Module where
    pp' (Module ds) = do
        res <- mapM pp' ds
        return $ vcat res

pp :: (Pretty p) => p -> String
pp p = show (runLFreshM . pp' $ p)

name :: Name a -> Doc
name = text . show
