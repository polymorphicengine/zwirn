{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Compiler
    ( HintEnv (..)
    , Environment (..)
    , compilerInterpreter
    , runCI
    ) where

import Zwirn.Language.Syntax
import Zwirn.Language.Simple
import qualified Zwirn.Language.Rotate as R
import Zwirn.Language.Parser
import Zwirn.Language.Block
import Zwirn.Language.Hint
import Zwirn.Language.Generator
import Zwirn.Language.TypeCheck.Types
import Zwirn.Language.TypeCheck.Env
import Zwirn.Language.TypeCheck.Infer

import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, modifyMVar_)
import Control.Exception (try, SomeException)

import Sound.Tidal.Context (Pattern, ControlPattern, Stream, streamReplace)
import Sound.Tidal.ID (ID(..))

import Data.Text (Text, unpack)
import Data.Text.IO (readFile)

import Prelude hiding (readFile)

data CIMessage = CIMessage Text deriving (Show, Eq)

data HintEnv = HintEnv { hMode :: HintMode
                       , hM :: MVar InterpreterMessage
                       , hR :: MVar InterpreterResponse
                       }



data Environment = Environment { tStream :: Stream
                               , jsMV :: Maybe (MVar (Pattern String))
                               , typeEnv :: TypeEnv
                               , hintEnv :: HintEnv
                               }

type CIError = String

type CI a = StateT Environment (ExceptT CIError IO) a

runCI :: Environment -> CI a -> IO (Either CIError a)
runCI env m = runExceptT $ evalStateT m env

compilerInterpreter :: Int -> Int -> Text -> CI (String, Environment, Int, Int)
compilerInterpreter line editor input = do
                       blocks <- runBlocks 0 input
                       (Block start end content) <- runGetBlock line blocks
                       as <- runParserWithPos start editor content
                       r <- runActions True as
                       e <- get
                       return (r, e, start, end)


-----------------------------------------------------
---------------------- Parser -----------------------
-----------------------------------------------------

runParserWithPos :: Int -> Int -> Text -> CI [Action]
runParserWithPos ln ed t = case parseActionsWithPos ln ed t of
                      Left err -> throwError err
                      Right as -> return as

runParser :: Text -> CI [Action]
runParser t = case parseActions t of
                      Left err -> throwError err
                      Right as -> return as

runBlocks :: Int -> Text -> CI [Block]
runBlocks ln t = case parseBlocks ln t of
                     Left err -> throwError err
                     Right bs -> return bs

runGetBlock :: Int -> [Block] -> CI Block
runGetBlock i bs = case getBlock i bs of
                    Left err -> throwError err
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
                      Left err -> throwError err
                      Right t -> return t


-----------------------------------------------------
-------------------- Type Check ---------------------
-----------------------------------------------------

runTypeCheck :: SimpleTerm -> CI Scheme
runTypeCheck s = do
              Environment {typeEnv = tenv} <- get
              case inferTerm tenv s of
                      Left err -> throwError $ show err
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

interpretAsControlPattern :: String -> CI ControlPattern
interpretAsControlPattern input = do
            (Environment{ hintEnv = (HintEnv _ hMV hRV) }) <- get
            liftIO $ putMVar hMV $ MPat input
            res <- liftIO $ takeMVar hRV
            case res of
              RPat p -> return p
              RError err -> throwError $ err
              _ -> throwError $ "Unkown Hint Error"

interpretAsStringPattern :: String -> CI (Pattern String)
interpretAsStringPattern input = do
            (Environment{ hintEnv = (HintEnv _ hMV hRV) }) <- get
            liftIO $ putMVar hMV $ MJS input
            res <- liftIO $ takeMVar hRV
            case res of
              RJS p -> return p
              RError err -> throwError $ err
              _ -> throwError $ "Unkown Hint Error"

interpretDefinition :: String -> CI ()
interpretDefinition input = do
            (Environment{ hintEnv = (HintEnv _ hMV hRV) }) <- get
            liftIO $ putMVar hMV $ MDef input
            res <- liftIO $ takeMVar hRV
            case res of
              RSucc -> return ()
              RError err -> throwError $ err
              _ -> throwError $ "Unkown Hint Error"

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
           interpretDefinition gd
           return ()

showAction :: Term -> CI String
showAction t = do
          s <- runSimplify t
          rot <- runRotate s
          ty <- runTypeCheck rot
          case isBasicType ty of
              False -> throwError $ "Can't show terms of type " ++ show ty
              True -> do
                gen <- runGenerator True rot
                cp <- interpretAsControlPattern gen
                return $ show cp

typeAction :: Term -> CI String
typeAction t = do
          s <- runSimplify t
          rot <- runRotate s
          ty <- runTypeCheck rot
          return $ show ty

loadAction :: Text -> CI ()
loadAction path = do
      mayfile <- liftIO ((try $ readFile $ unpack path) :: IO (Either SomeException Text))
      case mayfile of
        Left _ -> throwError "file note found"
        Right input -> do
          blocks <- runBlocks 0 input
          ass <- sequence $ map (runParser . bContent) blocks
          _ <- sequence $ map (runActions False) ass
          return ()

streamAction :: Bool -> Text -> Term -> CI ()
streamAction ctx idd t = do
              s <- runSimplify t
              rot <- runRotate s
              ty <- runTypeCheck rot
              case ty of
                  (Forall [] (Qual [] (TypeCon "ValueMap"))) -> do
                        gen <- runGenerator ctx rot
                        cp <- interpretAsControlPattern gen
                        (Environment {tStream = str}) <- get
                        liftIO $ streamReplace str (ID (unpack idd)) cp
                  _ -> throwError $ "Type Error: can only stream value maps"

jsAction :: Bool -> Term -> CI ()
jsAction ctx t = do
              s <- runSimplify t
              rot <- runRotate s
              ty <- runTypeCheck rot
              case ty of
                  (Forall [] (Qual [] (TypeCon "Text"))) -> do
                    gen <- runGenerator ctx rot
                    p <- interpretAsStringPattern gen
                    (Environment {jsMV = maybemv}) <- get
                    case maybemv of
                      Just mv -> liftIO $ modifyMVar_ mv (const $ pure p)
                      Nothing -> throwError $ "No JavaScript Interpreter available"
                  _ -> throwError $ "Type Error: can only accept text"


runAction :: Bool -> Action -> CI String
runAction b (Stream i t) = streamAction b i t >> return ""
runAction _ (Show t) = showAction t
runAction b (Def d) = defAction b d >> return ""
runAction _ (Type t) = typeAction t
runAction _ (Load p) = loadAction p >> return ""
runAction b (JS t) = jsAction b t >> return ""

runActions :: Bool -> [Action] -> CI String
runActions b as = fmap last $ sequence $ map (runAction b) as
