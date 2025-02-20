{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Compiler where

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
import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List (sortOn)
import Data.Text (Text, unpack)
import Data.Text.IO (readFile)
import Text.Read (readMaybe)
import Zwirn.Core.Types (silence)
import Zwirn.Language.Block
import Zwirn.Language.Builtin.Prelude (builtinNames)
import Zwirn.Language.Environment
import Zwirn.Language.Evaluate
import Zwirn.Language.Parser
import Zwirn.Language.Pretty
import qualified Zwirn.Language.Rotate as R
import Zwirn.Language.Simple
import Zwirn.Language.Syntax
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

data ConfigEnv
  = ConfigEnv
  { cConfigPath :: IO String,
    cResetConfig :: IO String
  }

data CiConfig = CiConfig
  { ciConfigOverwriteBuiltin :: Bool,
    ciConfigDynamicTypes :: Bool
  }

data Environment
  = Environment
  { tStream :: Stream,
    intEnv :: InterpreterEnv,
    confEnv :: Maybe ConfigEnv,
    currBlock :: Maybe CurrentBlock,
    ciConfig :: CiConfig,
    soundAction :: Text -> Zwirn Double -> CI ()
  }

data CIError
  = CIError
  { eError :: String,
    eEnv :: Environment
  }

instance Show CIError where
  show (CIError err _) = err

type CI = StateT Environment (ExceptT CIError IO)

runCI :: Environment -> CI a -> IO (Either CIError a)
runCI env m = runExceptT $ evalStateT m env

compilerInterpreterBasic :: Text -> CI String
compilerInterpreterBasic input = do
  as <- runParser input
  runActions True as

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
  env <- get
  throwError $ CIError err env

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
  Environment {intEnv = env} <- get
  case inferTerm env s of
    Left err -> throw $ show err
    Right t -> return t

-----------------------------------------------------
-------------------- Interpreter --------------------
-----------------------------------------------------

interpret :: SimpleTerm -> CI Expression
interpret input = do
  env <- gets intEnv
  return $ evaluate env input

-- if ctx is false, highlighting should be disabled
checkHighlight :: Bool -> Expression -> CI Expression
checkHighlight True x = return x
checkHighlight False x = return $ removePosExp x

-----------------------------------------------------
----------------- Checking Options  -----------------
-----------------------------------------------------

overwriteOk :: Text -> CI ()
overwriteOk name = do
  overwrite <- gets (ciConfigOverwriteBuiltin . ciConfig)
  when (not overwrite && name `elem` builtinNames) $ throw "Cannot overwrite builtin function. Please use OverwriteBuiltin."

dynamicOk :: Text -> Scheme -> CI ()
dynamicOk name ty = do
  dynamic <- gets (ciConfigDynamicTypes . ciConfig)
  mayty <- gets (lookupType name . intEnv)
  case mayty of
    Just oldType ->
      when (not dynamic && oldType /= ty) $ throw "Cannot overwrite definition with new type. Please use DynamicTypes."
    Nothing -> return ()

-----------------------------------------------------
----------------- Compiling Actions -----------------
-----------------------------------------------------

defAction :: Bool -> Def -> CI ()
defAction ctx d = do
  (LetS x st) <- runSimplifyDef d
  rot <- runRotate st
  ty <- runTypeCheck rot
  ex <- interpret rot
  exCtx <- checkHighlight ctx ex
  overwriteOk x
  dynamicOk x ty
  modify (\env -> env {intEnv = extend (x, exCtx, ty) (intEnv env)})

showAction :: Term -> CI String
showAction t = do
  s <- runSimplify t
  rot <- runRotate s
  ty <- runTypeCheck rot
  if isBasicType ty
    then do
      ex <- interpret rot
      stmv <- gets (sState . tStream)
      st <- liftIO $ readMVar stmv
      return $ showWithState st ex
    else throw $ "Can not show expressions of type: " ++ ppscheme ty

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
    Left _ -> throw "file not found"
    Right input -> do
      blocks <- runBlocks 0 input
      let sorted = sortOn (\(Block x _ _) -> x) blocks
      ass <- mapM (runParser . bContent) sorted
      mapM_ (runActions False) ass

infoAction :: Text -> CI String
infoAction n = do
  env <- gets intEnv
  case lookupFull n env of
    Just (Annotated _ t (Just d)) -> return $ unpack n ++ " :: " ++ ppscheme t ++ "\n" ++ unpack d
    Just (Annotated _ t Nothing) -> return $ unpack n ++ " :: " ++ ppscheme t
    Nothing -> throw $ "couldn't find information about " ++ unpack n

streamAction :: Bool -> Text -> Term -> CI ()
streamAction ctx key t = do
  s <- runSimplify t
  rot <- runRotate s
  ty <- runTypeCheck rot
  ex <- interpret rot
  exCtx <- checkHighlight ctx ex
  case ty of
    Forall _ (Qual _ (TypeVar _)) -> gets tStream >>= \str -> liftIO $ streamReplace str key (fromExp exCtx)
    Forall _ (Qual _ (TypeCon "Bus")) -> streamBus key exCtx
    Forall _ (Qual _ (TypeCon "Sound")) -> gets soundAction >>= \ac -> ac key (fromExp exCtx)
    Forall _ (Qual _ (TypeCon _)) -> gets tStream >>= \str -> liftIO $ streamReplace str key (fromExp exCtx)
    _ -> throw "Can only stream base types!"

streamBus :: Text -> Expression -> CI ()
streamBus key exCtx = do
  str <- gets tStream
  let mayindex = readMaybe $ unpack key
  case mayindex of
    Just ind -> liftIO $ streamReplaceBus str ind (fromExp exCtx)
    Nothing -> throw "Please use an integer as bus index."

streamSetAction :: Bool -> Text -> Term -> CI ()
streamSetAction ctx x t = do
  s <- runSimplify t
  rot <- runRotate s
  ty <- runTypeCheck rot
  ex <- interpret rot
  exCtx <- checkHighlight ctx ex
  overwriteOk x
  dynamicOk x ty
  setExpression x ty exCtx

setExpression :: Text -> Scheme -> Expression -> CI ()
setExpression x ty exCtx
  | isBasicType ty = do
      let newEx
            | isNumberT ty = EZwirn $ getStateN (pure x)
            | isTextT ty = EZwirn $ getStateT (pure x)
            | isMapT ty = EZwirn $ getStateM (pure x)
            | otherwise = EZwirn silence
      modify (\env -> env {intEnv = extend (x, newEx, ty) (intEnv env)})
      str <- gets tStream
      liftIO $ streamSet str x exCtx
  | otherwise = throw "Can only set basic types!"

streamOnceAction :: Bool -> Term -> CI ()
streamOnceAction ctx t = do
  s <- runSimplify t
  rot <- runRotate s
  ty <- runTypeCheck rot
  ex <- interpret rot
  exCtx <- checkHighlight ctx ex
  if isBasicType ty
    then
      ( do
          str <- gets tStream
          liftIO $ streamFirst str (fromExp exCtx)
      )
    else throw "Can only stream base types!"

streamSetTempoAction :: Tempo -> Text -> CI ()
streamSetTempoAction CPS t = gets tStream >>= \str -> liftIO $ streamSetCPS str (toRational (read $ unpack t :: Double))
streamSetTempoAction BPM t = gets tStream >>= \str -> liftIO $ streamSetBPM str (toRational (read $ unpack t :: Double))

resetConfigAction :: CI String
resetConfigAction = do
  (Environment {confEnv = mayEnv}) <- get
  case mayEnv of
    Nothing -> throw "Configuration not available."
    Just (ConfigEnv _ reset) -> liftIO reset

getConfigPathAction :: CI String
getConfigPathAction = do
  (Environment {confEnv = mayEnv}) <- get
  case mayEnv of
    Nothing -> throw "Configuration not available."
    Just (ConfigEnv path _) -> liftIO path

runAction :: Bool -> Action -> CI String
runAction b (StreamAction i t) = streamAction b i t >> return ""
runAction b (StreamSet i t) = streamSetAction b i t >> return ""
runAction b (StreamOnce t) = streamOnceAction b t >> return ""
runAction _ (StreamSetTempo mode t) = streamSetTempoAction mode t >> return ""
runAction _ (Show t) = showAction t
runAction b (Def d) = defAction b d >> return ""
runAction _ (Type t) = typeAction t
runAction _ (Load p) = loadAction p >> return ""
runAction _ (Info p) = infoAction p
runAction _ ConfigPath = getConfigPathAction
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
