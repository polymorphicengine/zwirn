{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Compiler
  ( EvalEnv,
    ConfigEnv (..),
    Environment (..),
    CIError (..),
    CurrentBlock (..),
    Stream,
    compilerInterpreterBlock,
    compilerInterpreterLine,
    compilerInterpreterWhole,
    compilerInterpreterBoot,
    runCI,
  )
where

{-
    Compiler.hs - implementation of a compiler-interpreter for zwirn
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

import Control.Concurrent (readMVar)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List (sortOn)
import Data.Text (Text, unpack)
import Data.Text.IO (readFile)
import Zwirn.Core.Cord (Cord)
import Zwirn.Core.Types (silence)
import Zwirn.Language.Block
import Zwirn.Language.Evaluate
import Zwirn.Language.Parser
import Zwirn.Language.Pretty
import qualified Zwirn.Language.Rotate as R
import Zwirn.Language.Simple
import Zwirn.Language.Syntax
import Zwirn.Language.TypeCheck.Env
import Zwirn.Language.TypeCheck.Infer
import Zwirn.Language.TypeCheck.Types
import Zwirn.Stream
import Prelude hiding (readFile)

newtype CIMessage
  = CIMessage Text
  deriving (Show, Eq)

data CurrentBlock
  = CurrentBlock Int Int
  deriving (Eq, Show)

type EvalEnv = ExpressionMap

data ConfigEnv
  = ConfigEnv
  { cSetConfig :: Text -> Text -> IO (),
    cResetConfig :: IO ()
  }

data Environment
  = Environment
  { tStream :: Stream,
    jsMV :: Maybe (MVar ()),
    typeEnv :: TypeEnv,
    evalEnv :: EvalEnv,
    confEnv :: Maybe ConfigEnv,
    currBlock :: Maybe CurrentBlock
  }

data CIError
  = CIError String (Maybe CurrentBlock)
  deriving (Eq, Show)

type CI a = StateT Environment (ExceptT CIError IO) a

runCI :: Environment -> CI a -> IO (Either CIError a)
runCI env m = runExceptT $ evalStateT m env

compilerInterpreterBlock :: Int -> Int -> Text -> CI (String, Environment, Int, Int)
compilerInterpreterBlock line editor input = do
  blocks <- runBlocks 0 input
  (Block strt end content) <- runGetBlock line blocks
  setCurrentBlock strt end
  as <- runParserWithPos strt editor content
  r <- runActions True as
  e <- get
  return (r, e, strt, end)

compilerInterpreterLine :: Int -> Int -> Text -> CI (String, Environment, Int, Int)
compilerInterpreterLine line editor input = do
  setCurrentBlock line line
  blocks <- runBlocks 0 input
  content <- runGetLine line blocks
  as <- runParserWithPos line editor content
  r <- runActions True as
  e <- get
  return (r, e, line, line)

compilerInterpreterWhole :: Int -> Text -> CI (String, Environment, Int, Int)
compilerInterpreterWhole editor input = do
  blocks <- runBlocks 0 input
  let sorted = sortOn (\(Block x _ _) -> x) blocks
      (Block strt _ _) = head sorted
      (Block _ end _) = last sorted
  liftIO $ print sorted
  setCurrentBlock strt end
  let parseBlock (Block s _ c) = runParserWithPos s editor c
  ass <- mapM parseBlock sorted
  rs <- mapM (runActions True) ass
  e <- get
  return (last rs, e, strt, end)

compilerInterpreterBoot :: [Text] -> CI Environment
compilerInterpreterBoot ps = runActions False (map Load ps) >> get

-----------------------------------------------------
----------------- Throwing Errors -------------------
-----------------------------------------------------

throw :: String -> CI a
throw err = do
  Environment {currBlock = b} <- get
  throwError $ CIError err b

setCurrentBlock :: Int -> Int -> CI ()
setCurrentBlock st en = modify (\env -> env {currBlock = Just $ CurrentBlock st en})

-----------------------------------------------------
---------------------- Parser -----------------------
-----------------------------------------------------

runParserWithPos :: Int -> Int -> Text -> CI [Action]
runParserWithPos ln ed t = case parseActionsWithPos ln ed t of
  Left err -> throw err
  Right as -> return as

runParser :: Text -> CI [Action]
runParser t = case parseActions t of
  Left err -> throw err
  Right as -> return as

runBlocks :: Int -> Text -> CI [Block]
runBlocks ln t = case parseBlocks ln t of
  Left err -> throw err
  Right bs -> return bs

runGetBlock :: Int -> [Block] -> CI Block
runGetBlock i bs = case getBlock i bs of
  Left err -> throw err
  Right b -> return b

runGetLine :: Int -> [Block] -> CI Text
runGetLine i bs = case getLn i bs of
  Left err -> throw err
  Right b -> return b

-----------------------------------------------------
---------------------- Desugar ----------------------
-----------------------------------------------------

runSimplify :: Term -> CI SimpleTerm
runSimplify t = return $ simplify t

runSimplifyDef :: Def -> CI SimpleDef
runSimplifyDef d = return $ simplifyDef d

-----------------------------------------------------
------------------- AST Rotation --------------------
-----------------------------------------------------

runRotate :: SimpleTerm -> CI SimpleTerm
runRotate s = case R.runRotate s of
  Left err -> throw err
  Right t -> return t

-----------------------------------------------------
-------------------- Type Check ---------------------
-----------------------------------------------------

runTypeCheck :: SimpleTerm -> CI Scheme
runTypeCheck s = do
  Environment {typeEnv = tenv} <- get
  case inferTerm tenv s of
    Left err -> throw $ show err
    Right t -> return t

-----------------------------------------------------
-------------------- Interpreter --------------------
-----------------------------------------------------

interpret :: SimpleTerm -> CI Expression
interpret input = do
  env <- gets evalEnv
  return $ evaluate env input

-----------------------------------------------------
----------------- Compiling Actions -----------------
-----------------------------------------------------

defAction :: Bool -> Def -> CI ()
defAction b d = do
  (LetS x st) <- runSimplifyDef d
  rot <- runRotate st
  ty <- runTypeCheck rot
  modify (\env -> env {typeEnv = extend (typeEnv env) (x, ty)})
  ex <- interpret rot
  modify (\env -> env {evalEnv = insert (x, ex) (evalEnv env)})

showAction :: Term -> CI String
showAction t = do
  s <- runSimplify t
  rot <- runRotate s
  ty <- runTypeCheck rot
  do
    ex <- interpret rot
    stmv <- gets (sState . tStream)
    st <- liftIO $ readMVar stmv
    return $ showWithState st ex

typeAction :: Term -> CI String
typeAction t = do
  s <- runSimplify t
  rot <- runRotate s
  ty <- runTypeCheck rot
  return $ ppTermHasType (t, ty)

loadAction :: Text -> CI ()
loadAction path = do
  mayfile <- liftIO ((try $ readFile $ unpack path) :: IO (Either SomeException Text))
  case mayfile of
    Left _ -> throw "file note found"
    Right input -> do
      blocks <- runBlocks 0 input
      let sorted = sortOn (\(Block x _ _) -> x) blocks
      ass <- mapM (runParser . bContent) sorted
      mapM_ (runActions False) ass

streamAction :: Bool -> Text -> Term -> CI ()
streamAction ctx _ t = do
  s <- runSimplify t
  rot <- runRotate s
  ty <- runTypeCheck rot
  p <- interpret rot
  if isBasicType ty
    then
      ( do
          str <- gets tStream
          liftIO $ streamReplace str $ fromExp p
      )
    else throw "Can only stream basic types!"

streamSetAction :: Bool -> Text -> Term -> CI ()
streamSetAction _ x t = do
  s <- runSimplify t
  rot <- runRotate s
  ty <- runTypeCheck rot
  ex <- interpret rot

  ( if isBasicType ty
      then
        ( do
            modify (\env -> env {typeEnv = extend (typeEnv env) (x, ty)})

            let newEx
                  | isNumberT ty = EZwirn $ getStateN (pure x)
                  | isTextT ty = EZwirn $ getStateT (pure x)
                  | isMapT ty = EZwirn $ getStateM (pure x)
                  | otherwise = EZwirn silence
            modify (\env -> env {evalEnv = insert (x, newEx) (evalEnv env)})

            -- put expression into state
            str <- gets tStream
            liftIO $ streamSet str x ex
        )
      else throw "Can only set basic types!"
    )

streamOnceAction :: Bool -> Term -> CI ()
streamOnceAction _ _ = throw "not implemented"

streamSetTempoAction :: Bool -> Tempo -> Term -> CI ()
streamSetTempoAction _ _ _ = throw "not implemented"

jsAction :: Bool -> Term -> CI ()
jsAction _ _ = throw "not implemented"

resetConfigAction :: CI String
resetConfigAction = do
  (Environment {confEnv = mayEnv}) <- get
  case mayEnv of
    Nothing -> throw "reset config not available"
    Just (ConfigEnv _ reset) -> liftIO $ reset >> return "configuration reset to default! please restart for it to have an effect!"

setConfigAction :: Text -> Text -> CI String
setConfigAction key v = do
  (Environment {confEnv = mayEnv}) <- get
  case mayEnv of
    Nothing -> throw "set config not available"
    Just (ConfigEnv setC _) ->
      ( if key `elem` tidalKeys
          then liftIO $ setC ("tidal." <> key) v >> return "configuration set! please restart for it to have an effect!"
          else
            ( if key `elem` editorKeys
                then liftIO $ setC ("editor." <> key) v >> return "configuration set! please restart for it to have an effect!"
                else (if key `elem` otherKeys then liftIO $ setC key v >> return "configuration set! please restart for it to have an effect!" else throw "unknown configuration key!")
            )
      )
  where
    tidalKeys = ["dirtport", "latency", "frameTimespan", "processAhead", "link", "skipTicks", "quantum", "beatsPerCycle"]
    editorKeys = ["lineNumbers", "keyMap", "matchBrackets", "autoCloseBrackets"]
    otherKeys = ["bootPath", "highlight", "hydra"]

runAction :: Bool -> Action -> CI String
runAction b (StreamAction i t) = streamAction b i t >> return ""
runAction b (StreamSet i t) = streamSetAction b i t >> return ""
runAction b (StreamOnce t) = streamOnceAction b t >> return ""
runAction b (StreamSetTempo mode t) = streamSetTempoAction b mode t >> return ""
runAction _ (Show t) = showAction t
runAction b (Def d) = defAction b d >> return ""
runAction _ (Type t) = typeAction t
runAction _ (Load p) = loadAction p >> return ""
runAction b (JS t) = jsAction b t >> return ""
runAction _ (Config k v) = setConfigAction k v
runAction _ ResetConfig = resetConfigAction

runActions :: Bool -> [Action] -> CI String
runActions b as = last <$> mapM (runAction b) as

isNumberT :: Scheme -> Bool
isNumberT (Forall _ (Qual _ (TypeCon "Number"))) = True
isNumberT _ = False

isTextT :: Scheme -> Bool
isTextT (Forall _ (Qual _ (TypeCon "Text"))) = True
isTextT _ = False

isMapT :: Scheme -> Bool
isMapT (Forall _ (Qual _ (TypeCon "Map"))) = True
isMapT _ = False
