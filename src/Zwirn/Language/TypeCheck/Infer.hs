{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.TypeCheck.Infer
    ( inferTerm
    , defaultEnv
    ) where

import Zwirn.Language.TypeCheck.Types
import Zwirn.Language.TypeCheck.Constraint
import Zwirn.Language.TypeCheck.Env as Env
import Zwirn.Language.Simple

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import           Data.Text (Text, pack)

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Inference monad
type Infer a = (ReaderT
                  TypeEnv             -- Typing environment
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
runInfer :: TypeEnv -> Infer a -> Either TypeError a
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferTerm :: TypeEnv -> SimpleTerm -> Either TypeError Scheme
inferTerm env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, ps, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> case runInfer env (filterAndCheck (apply subst ps) (apply subst ty)) of
        Left err -> Left err
        Right xs -> Right $ closeOver xs $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
-- constraintsTerm :: Env -> SimpleTerm -> Either TypeError ([Constraint], Subst, Type, Scheme)
-- constraintsTerm env ex = case runInfer env (infer ex) of
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

generalize :: TypeEnv -> [Predicate] -> Type -> Scheme
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

infer :: SimpleTerm -> Infer (Type, [Predicate], [Constraint])
infer expr = case expr of
  SVar _ x  -> do
    (t, ps) <- lookupEnv x
    return (t, ps, [])

  SText _ _ -> return (textT, [], [])

  SNum _ _ -> return (numberT, [], [])

  SRest -> do
    tv <- fresh
    return (tv, [], [])

  SLambda x e -> do
    tv <- fresh
    (t, ps, c) <- inEnv (x, Forall [] (Qual [] tv)) (infer e)
    return (tv `TypeArr` t, ps, c)

  SApp e1 e2 -> do
    (t1, ps1, c1) <- infer e1
    (t2, ps2, c2) <- infer e2
    tv <- fresh
    return (tv, ps1 ++ ps2, c1 ++ c2 ++ [(t1, t2 `TypeArr` tv)])

  SInfix e1 op e2 -> do
    (t1, ps1, c1) <- infer e1
    (t2, ps2, c2) <- infer e2
    tv <- fresh
    let u1 = t1 `TypeArr` (t2 `TypeArr` tv)
    (u2, p3) <- lookupEnv op
    return (tv, ps1 ++ ps2 ++ p3, c1 ++ c2 ++ [(u1, u2)])

  SSeq (x:xs) -> do
    (t, ps, cs) <- infer x
    infs <- sequence $ map infer xs
    return (t, ps, cs ++ concatMap (\(_,_,y) -> y) infs ++ [(t,t') | t' <- map (\(y,_,_) -> y) infs])

  SStack (x:xs) -> do
    (t, ps, cs) <- infer x
    infs <- sequence $ map infer xs
    return (t, ps, cs ++ concatMap (\(_,_,y) -> y) infs ++ [(t,t') | t' <- map (\(y,_,_) -> y) infs])

  SChoice _ (x:xs) -> do
    (t, ps, cs) <- infer x
    infs <- sequence $ map infer xs
    return (t, ps, cs ++ concatMap (\(_,_,y) -> y) infs ++ [(t,t') | t' <- map (\(y,_,_) -> y) infs])

  SElong x _ -> infer x

  (SEuclid e1 e2 e3 (Just e4)) -> do
    (t1, ps1, c1) <- infer e1
    (t2, ps2, c2) <- infer e2
    (t3, ps3, c3) <- infer e3
    (t4, ps4, c4) <- infer e4
    return (t1, ps1 ++ ps2 ++ ps3 ++ ps4, c1 ++ c2 ++ c3 ++ c4 ++ [(t2, numberT), (t3, numberT), (t4, numberT)])

  (SEuclid e1 e2 e3 Nothing) -> do
    (t1, ps1, c1) <- infer e1
    (t2, ps2, c2) <- infer e2
    (t3, ps3, c3) <- infer e3
    return (t1, ps1 ++ ps2 ++ ps3, c1 ++ c2 ++ c3 ++ [(t2, numberT), (t3, numberT)])

  _ -> error "Can't happen"

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


tyv :: Text -> Type
tyv s = TypeVar s

num :: Type -> Predicate
num t = IsIn "Num" t

infixr 3 -->
(-->) :: Type -> Type -> Type
(-->) t1 t2 = TypeArr t1 t2

defaultEnv :: TypeEnv
defaultEnv = TypeEnv (Map.fromList [("rev", Forall ["a"] (Qual [] $ tyv "a" --> tyv "a"))
                                   ,("fast", Forall ["a"] (Qual [] $ numberT --> tyv "a" --> tyv "a"))
                                   ,("*", Forall ["a"] (Qual [] $ tyv "a" --> numberT --> tyv "a"))
                                   ,("slow", Forall ["a"] (Qual [] $ numberT --> tyv "a" --> tyv "a"))
                                   ,("/", Forall ["a"] (Qual [] $ tyv "a" --> numberT --> tyv "a"))
                                   ,("id",Forall ["a"] (Qual [] $ tyv "a" --> tyv "a"))
                                   ,("n",Forall [] (Qual [] $ numberT --> valMapT))
                                   ,("s",Forall [] (Qual [] $ textT --> valMapT))
                                   ,("bd",Forall [] (Qual [] $ textT))
                                   ,("sn",Forall [] (Qual [] $ textT))
                                   ,("out",Forall [] (Qual [] $ numberT --> textT --> textT))
                                   ,("osc",Forall [] (Qual [] $ numberT --> textT))
                                   ])
                         [IsIn "Num" numberT]
