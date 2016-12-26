{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Term where
import           Control.Monad.Trans
import qualified Data.Map                         as Map
import           Data.Typeable                    (Typeable)
import           GHC.Generics

import           Unbound.Generics.LocallyNameless

data Term = Data -- type of types ( finite )
          | Codata -- type of types ( infinite (corecursive) )
          | Var (Name Term)
          | Lambda (Bind (Name Term) Term)
          | Pi (Bind (Name Term) Term) -- "forall"
          | App Term Term
        deriving (Show, Generic, Typeable)


instance Alpha Term

instance Subst Term Term where
    isvar (Var x) = Just ( SubstName x)
    isvar _       = Nothing

data Declaration = Type (Name Term)
                 | Def (Name Term)
        deriving (Show, Generic, Typeable)

type Module = [Declaration]
