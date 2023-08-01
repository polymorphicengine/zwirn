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
import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Exception (try, SomeException)

import Sound.Tidal.Context (Pattern, ControlPattern)

import Data.Text (Text, unpack)
import Data.Text.IO (readFile)

import Prelude hiding (readFile)

data CIMessage = CIMessage Text deriving (Show, Eq)

data HintEnv = HintEnv { hMode :: HintMode
                       , hM :: MVar InterpreterMessage
                       , hR :: MVar InterpreterResponse
                       }

data Environment = Environment { typeEnv :: TypeEnv
                               , hintEnv :: HintEnv
                               }

type CIError = String

type CI a = StateT Environment (ExceptT CIError IO) a

runCI :: Environment -> CI a -> IO (Either CIError a)
runCI env m = runExceptT $ evalStateT m env


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

runParserWithPos :: Int -> Int -> Text -> CI [Action]
runParserWithPos ed ln t = case parseActionsWithPos ed ln t of
                      Left err -> throwError err
                      Right as -> return as

runParser :: Text -> CI [Action]
runParser t = case parseActions t of
                      Left err -> throwError err
                      Right as -> return as


runBlocks :: Text -> CI [Block]
runBlocks t = case parseBlocks t of
                     Left err -> throwError err
                     Right bs -> return bs

runGetBlock :: Int -> [Block] -> CI Block
runGetBlock i bs = case getBlock i bs of
                    Left err -> throwError err
                    Right b -> return b

runSimplify :: Term -> CI SimpleTerm
runSimplify t = return $ simplify t

runSimplifyDef :: Def -> CI SimpleDef
runSimplifyDef d = return $ simplifyDef d

runRotate :: SimpleTerm -> CI SimpleTerm
runRotate s = case R.runRotate s of
                      Left err -> throwError err
                      Right t -> return t

runTypeCheck :: SimpleTerm -> CI Scheme
runTypeCheck s = do
              Environment {typeEnv = tenv} <- get
              case inferTerm tenv s of
                      Left err -> throwError $ show err
                      Right t -> return t

runGenerator :: SimpleTerm -> CI String
runGenerator s = return $ generate s

runGeneratorDef :: SimpleDef -> CI String
runGeneratorDef s = return $ generateDef s

defAction :: Def -> CI ()
defAction d = do
           sd@(LetS x st) <- runSimplifyDef d
           rot <- runRotate st
           ty <- runTypeCheck rot
           modify (\env -> env{typeEnv = extend (typeEnv env) (x, ty)})
           gd <- runGeneratorDef sd
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
                gen <- runGenerator rot
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
          blocks <- runBlocks input
          ass <- sequence $ map (runParser . bContent) blocks
          _ <- sequence $ map runActions ass                  -- TODO: versions without context
          return ()


runAction :: Action -> CI String
runAction (Show t) = showAction t
runAction (Def d) = defAction d >> return ""
runAction (Type t) = typeAction t
runAction (Load p) = loadAction p >> return ""

runActions :: [Action] -> CI String
runActions as = fmap last $ sequence $ map runAction as

compilerInterpreter :: Text -> CI (String, Environment)
compilerInterpreter input = do
                       as <- runParserWithPos 0 1 input
                       r <- runActions as
                       e <- get
                       return (r,e)
