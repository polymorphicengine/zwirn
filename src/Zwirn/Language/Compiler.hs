{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Zwirn.Language.Compiler
    ( HintEnv (..)
    , ConfigEnv (..)
    , Environment (..)
    , CIError (..)
    , CurrentBlock (..)
    , compilerInterpreterBlock
    , compilerInterpreterLine
    , compilerInterpreterWhole
    , compilerInterpreterBoot
    , runCI
    ) where

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

import Zwirn.Language.Syntax
import Zwirn.Language.Simple
import qualified Zwirn.Language.Rotate as R
import Zwirn.Language.Parser
import Zwirn.Language.Block
import Zwirn.Language.Hint
import Zwirn.Language.Generator
import Zwirn.Language.Pretty
import Zwirn.Language.TypeCheck.Types
import Zwirn.Language.TypeCheck.Env
import Zwirn.Language.TypeCheck.Infer

import Zwirn.Interactive.Types (TextPattern, NumberPattern)
import Zwirn.Interactive.Convert (_fromTarget)

import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, modifyMVar_, readMVar)
import Control.Exception (try, SomeException)

import Sound.Tidal.Context (ControlPattern, Stream, State (..), ArcF (..), streamReplace, streamSet, streamOnce, cps, (#), orbit, query, sStateMV)
import Sound.Tidal.ID (ID(..))

import Data.Text (Text, unpack)
import Data.Text.IO (readFile)

import Data.List (sortOn, intercalate)
import Text.Read (readMaybe)

import Prelude hiding (readFile)

data CIMessage
  = CIMessage Text deriving (Show, Eq)

data CurrentBlock
  = CurrentBlock Int Int
  deriving (Eq, Show)

data HintEnv
  = HintEnv { hMode :: HintMode
            , hM :: MVar InterpreterMessage
            , hR :: MVar InterpreterResponse
            }

data ConfigEnv
  = ConfigEnv { cSetConfig :: Text -> Text -> IO ()
              , cResetConfig :: IO ()
              }


data Environment
  = Environment { tStream :: Stream
                , jsMV :: Maybe (MVar TextPattern)
                , typeEnv :: TypeEnv
                , hintEnv :: HintEnv
                , confEnv :: Maybe ConfigEnv
                , currBlock :: Maybe CurrentBlock
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
                      let sorted = sortOn (\(Block x _ _ ) -> x) blocks
                          (Block strt _ _) = head sorted
                          (Block _ end _) = last sorted
                      liftIO $ putStrLn $ show sorted
                      setCurrentBlock strt end
                      let parseBlock (Block s _ c) = runParserWithPos s editor c
                      ass <- sequence $ map parseBlock sorted
                      rs <- sequence $ map (runActions True) ass
                      e <- get
                      return (last rs, e, strt, end)

compilerInterpreterBoot :: [Text] -> CI Environment
compilerInterpreterBoot ps = do
                        _ <- runActions False (map Load ps)
                        e <- get
                        return e


-----------------------------------------------------
----------------- Throwing Errors -------------------
-----------------------------------------------------

throw :: String -> CI a
throw err = do
  Environment {currBlock = b} <- get
  throwError $ CIError err b

setCurrentBlock :: Int -> Int -> CI ()
setCurrentBlock st en = modify (\env -> env{currBlock = Just $ CurrentBlock st en})

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
-------------- Generate Haskell Code ----------------
-----------------------------------------------------

-- | True runs the generator with inserting context, False without
runGenerator :: Bool -> SimpleTerm -> CI String
runGenerator True s = return $ generate s
runGenerator False s = return $ generateWithoutContext s

-- | True runs the generator with inserting context, False without
runGeneratorDef :: Bool -> SimpleDef -> CI String
runGeneratorDef True s = return $ generateDef s
runGeneratorDef False s = return $ generateDefWithoutContext s

-----------------------------------------------------
---------------- Haskell interpreter ----------------
-----------------------------------------------------

interpret :: FromResponse a => MessageType -> String -> CI a
interpret typ input = do
            (Environment{ hintEnv = (HintEnv _ hMV hRV) }) <- get
            liftIO $ putMVar hMV $ Message typ input
            res <- liftIO $ takeMVar hRV
            case fromResponse res of
              Left err -> throw $ "GHC Error: " ++ err
              Right a -> return $ a

-----------------------------------------------------
----------------- Compiling Actions -----------------
-----------------------------------------------------

defAction :: Bool -> Def -> CI ()
defAction b d = do
           sd@(LetS x st) <- runSimplifyDef d
           rot <- runRotate st
           ty <- runTypeCheck rot
           modify (\env -> env{typeEnv = extend (typeEnv env) (x, ty)})
           gd <- runGeneratorDef b sd
           interpret @() AsDef gd
           return ()

showAction :: Term -> CI String
showAction t = do
          s <- runSimplify t
          rot <- runRotate s
          ty <- runTypeCheck rot
          (Environment {tStream = str}) <- get
          sMap <- liftIO $ readMVar (sStateMV str)
          case ty of
            (Forall [] (Qual [] (TypeCon "Number"))) -> do
                      gen <- runGenerator True rot
                      cp <- interpret @NumberPattern AsNum gen
                      return $ intercalate "\n" $ map show $ query cp (State (Arc 0 1) sMap)
            (Forall [] (Qual [] (TypeCon "Text"))) -> do
                      gen <- runGenerator True rot
                      cp <- interpret @TextPattern AsText gen
                      return $ intercalate "\n" $ map show $ query cp (State (Arc 0 1) sMap)
            (Forall [] (Qual [] (TypeCon "ValueMap"))) -> do
                      gen <- runGenerator True rot
                      cp <- interpret @ControlPattern AsVM gen
                      return $ intercalate "\n" $ map show $ query cp (State (Arc 0 1) sMap)
            _ -> throw $ "Can't show terms of type " ++ ppscheme ty


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
          let sorted = sortOn (\(Block x _ _ ) -> x) blocks
          ass <- sequence $ map (runParser . bContent) sorted
          _ <- sequence $ map (runActions False) ass
          return ()

streamAction :: Bool -> Text -> Term -> CI ()
streamAction ctx idd t = do
              s <- runSimplify t
              rot <- runRotate s
              ty <- runTypeCheck rot
              case isVM ty of
                  True -> do
                      gen <- runGenerator ctx rot
                      cp <- interpret AsVM gen
                      (Environment {tStream = str}) <- get
                      cp' <- resolveOrbits (unpack idd) cp
                      liftIO $ streamReplace str (ID (unpack idd)) cp'
                  False -> throw $ "Type Error: can only stream value maps"

streamSetAction :: Bool -> Text -> Term -> CI ()
streamSetAction ctx idd t = do
              s <- runSimplify t
              rot <- runRotate s
              ty <- runTypeCheck rot
              gen <- runGenerator ctx rot
              (Environment {tStream = str}) <- get
              case ty of
                  (Forall [] (Qual [] (TypeCon "Number"))) -> do
                        modify (\env -> env{typeEnv = extend (typeEnv env) (idd, ty)})
                        interpret @() AsDef $ "let " ++ unpack idd ++ "= _cX' (Num 0) _valToNum " ++ ("\"" ++ unpack idd ++ "\"")
                        np <- interpret @NumberPattern AsNum gen
                        liftIO $ streamSet str (unpack idd) np
                  (Forall [] (Qual [] (TypeCon "Text"))) -> do
                        modify (\env -> env{typeEnv = extend (typeEnv env) (idd, ty)})
                        interpret @() AsDef $ "let " ++ unpack idd ++ "= _cX' (Text \"\") _valToText " ++ ("\"" ++ unpack idd ++ "\"")
                        tp <- interpret @TextPattern AsText gen
                        liftIO $ streamSet str (unpack idd) tp
                  (Forall [] (Qual [] (TypeCon "ValueMap"))) -> do
                        modify (\env -> env{typeEnv = extend (typeEnv env) (idd, ty)})
                        interpret @() AsDef $ "let " ++ unpack idd ++ "= _cX' _emptyVM _valToVM " ++ ("\"" ++ unpack idd ++ "\"")
                        tp <- interpret @ControlPattern AsVM gen
                        liftIO $ streamSet str (unpack idd) tp
                  (Forall _ (Qual [] (TypeVar _))) -> do
                        modify (\env -> env{typeEnv = extend (typeEnv env) (idd, ty)})
                        interpret @() AsDef $ "let " ++ unpack idd ++ "= _cX' _emptyVM _valToVM " ++ ("\"" ++ unpack idd ++ "\"")
                        tp <- interpret @ControlPattern AsVM gen
                        liftIO $ streamSet str (unpack idd) tp
                  _ -> throw $ "Type Error: can only set basic patterns"

streamOnceAction :: Bool -> Term -> CI ()
streamOnceAction ctx t = do
              s <- runSimplify t
              rot <- runRotate s
              ty <- runTypeCheck rot
              case ty of
                  (Forall [] (Qual [] (TypeCon "ValueMap"))) -> do
                        gen <- runGenerator ctx rot
                        cp <- interpret AsVM gen
                        (Environment {tStream = str}) <- get
                        liftIO $ streamOnce str cp
                  _ -> throw $ "Type Error: can only stream value maps"

streamSetTempoAction :: Bool -> Tempo -> Term -> CI ()
streamSetTempoAction ctx tempo t = do
                      s <- runSimplify t
                      rot <- runRotate s
                      ty <- runTypeCheck rot
                      case ty of
                          (Forall [] (Qual [] (TypeCon "Number"))) -> do
                                gen <- runGenerator ctx rot
                                np <- interpret @NumberPattern AsNum gen
                                (Environment {tStream = str}) <- get
                                case tempo of
                                  CPS -> liftIO $ streamOnce str $ cps (_fromTarget np)
                                  BPM -> liftIO $ streamOnce str $ cps (fmap (\x -> x/60/4) $ _fromTarget np)
                          _ -> throw $ "Type Error: tempo must be a number"

jsAction :: Bool -> Term -> CI ()
jsAction ctx t = do
              s <- runSimplify t
              rot <- runRotate s
              ty <- runTypeCheck rot
              gen <- runGenerator ctx rot
              case ty of
                  (Forall [] (Qual [] (TypeCon "Text"))) -> do
                    p <- interpret @TextPattern AsText gen
                    (Environment {jsMV = maybemv}) <- get
                    case maybemv of
                      Just mv -> liftIO $ modifyMVar_ mv (const $ pure p)
                      Nothing -> throw $ "No JavaScript Interpreter available"
                  (Forall _ (Qual [] (TypeVar _))) -> do
                    p <- interpret @TextPattern AsText gen
                    (Environment {jsMV = maybemv}) <- get
                    case maybemv of
                      Just mv -> liftIO $ modifyMVar_ mv (const $ pure p)
                      Nothing -> throw $ "No JavaScript Interpreter available"
                  _ -> throw $ "Type Error: can only accept text"

resetConfigAction :: CI String
resetConfigAction = do
  (Environment {confEnv = mayEnv}) <- get
  case mayEnv of
    Nothing -> throw $ "reset config not available"
    Just (ConfigEnv _ reset) -> liftIO $ reset >> return "configuration reset to default! please restart for it to have an effect!"

setConfigAction :: Text -> Text -> CI String
setConfigAction key v = do
  (Environment {confEnv = mayEnv}) <- get
  case mayEnv of
    Nothing -> throw $ "set config not available"
    Just (ConfigEnv setC _) -> case elem key tidalKeys of
                                     False -> case elem key editorKeys of
                                                   False -> case elem key otherKeys of
                                                     False -> throw $ "unknown configuration key!"
                                                     True -> liftIO $ setC key v >> return "configuration set! please restart for it to have an effect!"
                                                   True -> liftIO $ setC ("editor." <> key) v >> return "configuration set! please restart for it to have an effect!"
                                     True -> liftIO $ setC ("tidal." <> key) v >> return "configuration set! please restart for it to have an effect!"
  where tidalKeys = ["dirtport", "latency", "frameTimespan", "processAhead", "link", "skipTicks", "quantum", "beatsPerCycle"]
        editorKeys = ["lineNumbers", "keyMap", "matchBrackets", "autoCloseBrackets"]
        otherKeys = ["bootPath", "highlight", "hydra"]

runAction :: Bool -> Action -> CI String
runAction b (Stream i t) = streamAction b i t >> return ""
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
runActions b as = fmap last $ sequence $ map (runAction b) as

-----------------------------------------------------
-------------------- Utilities ----------------------
-----------------------------------------------------



resolveOrbits :: String -> ControlPattern -> CI ControlPattern
resolveOrbits t cp = case ((readMaybe t) :: Maybe Int) of
                              Just o -> return $ cp # orbit (pure $ o-1)
                              Nothing -> return cp

isVM :: Scheme -> Bool
isVM (Forall _ (Qual _ (TypeCon "ValueMap"))) = True
isVM (Forall _ (Qual [] (TypeVar _))) = True
isVM _ = False
