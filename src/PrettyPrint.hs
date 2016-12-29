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
    pp' (TVar n) = return $ blue (name n)
    pp' (TArr t1 t2) = do
          p1 <- pp' t1
          p2 <- pp' t2
          return $ text "\\" <> blue p1 <+> farrow <+> blue p2
    pp' TData = return $ blue ( text "Data" )

instance Pretty Term where
    -- pp' p | trace ("pp was called: " ++ show p) False = undefined
    -- pp' (Base c)    = pp' c
    pp' (Var n) = return $ name n
    pp' (App e1 e2) = do
        p1 <- pp' e1
        p2 <- pp' e2
        return $ parens $ p1 <+> p2
    pp' (Lambda b) = lunbind b $ \(n, e) -> do
        pe <- pp' e
        return $ backslash <> green (name n) <+> farrow <+> green (pe)
    pp' Data = return $ green ( text "Data" )
-- instance Pretty Base where
--     pp' Data   = return $ text "Data"
--     pp' Codata = return $ text "Codata"
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
