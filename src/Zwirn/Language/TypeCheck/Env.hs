module Zwirn.Language.TypeCheck.Env where

import Prelude hiding (lookup)
import Data.Monoid ()
import Data.Foldable hiding (toList)
import qualified Data.Map as Map

import Zwirn.Language.TypeCheck.Types

data Env
  = TypeEnv { types :: Map.Map Name Scheme
            , instances :: [Instance]
            }
  deriving (Eq, Show)

empty :: Env
empty = TypeEnv Map.empty []

extend :: Env -> (Name, Scheme) -> Env
extend env (x, s) = env { types = Map.insert x s (types env) }

remove :: Env -> Name -> Env
remove (TypeEnv ty cl) var = TypeEnv (Map.delete var ty) cl

extends :: Env -> [(Name, Scheme)] -> Env
extends env xs = env { types = Map.union (Map.fromList xs) (types env) }

lookup :: Name -> Env -> Maybe Scheme
lookup key (TypeEnv tys _ ) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv ty1 cl1) (TypeEnv ty2 cl2) = TypeEnv (Map.union ty1 ty2) (cl1 ++ cl2)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: Name -> Scheme -> Env
singleton x y = TypeEnv (Map.singleton x y) []

keys :: Env -> [Name]
keys (TypeEnv env _) = Map.keys env

fromList :: [(Name, Scheme)] -> Env
fromList xs = TypeEnv (Map.fromList xs) []

toList :: Env -> [(Name, Scheme)]
toList (TypeEnv env _) = Map.toList env

instance Semigroup Env where
  (<>) = merge

instance Monoid Env where
  mempty = empty
