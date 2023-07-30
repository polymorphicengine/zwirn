module Zwirn.Language.TypeCheck.Env where

import Prelude hiding (lookup)
import Data.Monoid ()
import Data.Foldable hiding (toList)
import qualified Data.Map as Map

import Zwirn.Language.TypeCheck.Types

data TypeEnv
  = TypeEnv { types :: Map.Map Name Scheme
            , instances :: [Instance]
            }
  deriving (Eq, Show)

empty :: TypeEnv
empty = TypeEnv Map.empty []

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend env (x, s) = env { types = Map.insert x s (types env) }

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv ty cl) var = TypeEnv (Map.delete var ty) cl

extends :: TypeEnv -> [(Name, Scheme)] -> TypeEnv
extends env xs = env { types = Map.union (Map.fromList xs) (types env) }

lookup :: Name -> TypeEnv -> Maybe Scheme
lookup key (TypeEnv tys _ ) = Map.lookup key tys

merge :: TypeEnv -> TypeEnv -> TypeEnv
merge (TypeEnv ty1 cl1) (TypeEnv ty2 cl2) = TypeEnv (Map.union ty1 ty2) (cl1 ++ cl2)

mergeEnvs :: [TypeEnv] -> TypeEnv
mergeEnvs = foldl' merge empty

singleton :: Name -> Scheme -> TypeEnv
singleton x y = TypeEnv (Map.singleton x y) []

keys :: TypeEnv -> [Name]
keys (TypeEnv env _) = Map.keys env

fromList :: [(Name, Scheme)] -> TypeEnv
fromList xs = TypeEnv (Map.fromList xs) []

toList :: TypeEnv -> [(Name, Scheme)]
toList (TypeEnv env _) = Map.toList env

instance Semigroup TypeEnv where
  (<>) = merge

instance Monoid TypeEnv where
  mempty = empty
