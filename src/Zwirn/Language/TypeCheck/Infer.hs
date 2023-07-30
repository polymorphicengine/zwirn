module Zwirn.Language.TypeCheck.Infer
    ( inferExpr
    ) where

import Zwirn.Language.TypeCheck.Types
import Zwirn.Language.TypeCheck.Constraint
import Zwirn.Language.TypeCheck.Env as Env
import Zwirn.Language.Syntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import           Data.Text (Text, pack)
import qualified Data.Text as Text

import Text.Read (readMaybe)

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Inference monad
type Infer a = (ReaderT
                  Env             -- Typing environment
                  (StateT         -- Inference state
                  InferState
                  (Except         -- Inference errors
                    TypeError))
                  a)              -- Result

-- | Inference state
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer a -> Either TypeError a
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> Term -> Either TypeError Scheme
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, ps, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> case runInfer env (filterAndCheck (apply subst ps) (apply subst ty)) of
        Left err -> Left err
        Right xs -> Right $ closeOver xs $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
-- constraintsExpr :: Env -> Term -> Either TypeError ([Constraint], Subst, Type, Scheme)
-- constraintsExpr env ex = case runInfer env (infer ex) of
--   Left err -> Left err
--   Right (ty, cs) -> case runSolve cs of
--     Left err -> Left err
--     Right subst -> Right (cs, subst, ty, sc)
--       where
--         sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: [Predicate] -> Type -> Scheme
closeOver ps t = normalize $ generalize Env.empty ps t

-- | Extend type environment
inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = (remove e x) `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupEnv :: Name -> Infer (Type, [Predicate])
lookupEnv x = do
  (TypeEnv env _) <- ask
  case Map.lookup x env of
      Nothing   ->  throwError $ UnboundVariable x
      Just s    ->  do t <- instantiate s
                       return t

letters :: [Text]
letters = map pack $ [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TypeVar (letters !! count s)

instantiate ::  Scheme -> Infer (Type, [Predicate])
instantiate (Forall as (Qual ps t)) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ (apply s t, apply s ps)

generalize :: Env -> [Predicate] -> Type -> Scheme
generalize env ps t  = Forall as (Qual ps t)
    where as = Set.toList $ ftv t `Set.difference` ftv env

filterAndCheck :: [Predicate] -> Type -> Infer [Predicate]
filterAndCheck [] _ = return []
filterAndCheck (p@(IsIn _ (TypeVar _)):ps) t
        = case or $ Set.map (\x -> elem x $ ftv p) (ftv t) of
            True -> fmap (p:) $ filterAndCheck ps t
            False -> filterAndCheck ps t
filterAndCheck (p:ps) t = checkInstance p >> filterAndCheck ps t

checkInstance :: Predicate -> Infer ()
checkInstance p = do
                (TypeEnv _ is) <- ask
                case elem p is of
                        True -> return ()
                        False -> throwError $ NoInstance p

infer :: Term -> Infer (Type, [Predicate], [Constraint])
infer expr = case expr of
  TVar _ x  -> do
      case (readMaybe $ Text.unpack x) :: Maybe Double of
        Nothing -> do
            (t, ps) <- lookupEnv x
            return (t, ps, [])
        Just _ -> return (numberT, [], [])

  TRest -> do
      tv <- fresh
      return (tv, [], [])

  TLambda [x] e -> do --TODO: for more than one variable
    tv <- fresh
    (t, ps, c) <- inEnv (x, Forall [] (Qual [] tv)) (infer e)
    return (tv `TypeArr` t, ps, c)

  TApp e1 e2 -> do
    (t1, ps1, c1) <- infer e1
    (t2, ps2, c2) <- infer e2
    tv <- fresh
    return (tv, ps1 ++ ps2, c1 ++ c2 ++ [(t1, t2 `TypeArr` tv)])

  TInfix e1 op e2 -> do
    (t1, ps1, c1) <- infer e1
    (t2, ps2, c2) <- infer e2
    tv <- fresh
    let u1 = t1 `TypeArr` (t2 `TypeArr` tv)
    (u2, p3) <- lookupEnv op
    return (tv, ps1 ++ ps2 ++ p3, c1 ++ c2 ++ [(u1, u2)])

  TSeq (x:xs) -> do
    (t, ps, cs) <- infer x
    infs <- sequence $ map infer xs
    return (t, ps, cs ++ concatMap (\(_,_,y) -> y) infs ++ [(t,t') | t' <- map (\(y,_,_) -> y) infs])


normalize :: Scheme -> Scheme
normalize (Forall _ (Qual ps body)) = Forall (map snd ord) (Qual (map normpred ps) $ normtype body)
  where
    ord = zip (nub $ fv body) letters

    fv (TypeVar a)   = [a]
    fv (TypeArr a b) = fv a ++ fv b
    fv (TypeCon _)    = []

    normtype (TypeArr a b) = TypeArr (normtype a) (normtype b)
    normtype (TypeCon a)   = TypeCon a
    normtype (TypeVar a)   =
      case Prelude.lookup a ord of
        Just x -> TypeVar x
        Nothing -> error "type variable not in signature"

    normpred (IsIn n t) = IsIn n (normtype t)
