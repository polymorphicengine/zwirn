module Zwirn.Language.TypeCheck.Env where

{-
    Env.hs - type inference environment adapted from
    https://github.com/sdiehl/write-you-a-haskell/tree/master/chapter7/poly_constraints
    Copyright (C) 2023, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

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
