module Editor.Backend where

import Control.Monad  (void)

import qualified Sound.Tidal.Context as T (Stream, streamReplace)

import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)
import Control.Exception (try, SomeException)

import Foreign.JavaScript (JSObject)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Text.Megaparsec (errorBundlePretty)

import Editor.Hint
import Editor.Block
import Editor.UI
import Megaparsec
import Compiler
import Language

data Env = Env {windowE :: Window
               ,streamE :: T.Stream
               ,hintM :: MVar InterpreterMessage
               ,hintR :: MVar InterpreterResponse
               }

data ActionResponse = ASucc String
                    | AErr String
                    deriving (Eq,Show)

interpretCommands :: JSObject -> Bool -> Env -> UI ()
interpretCommands cm lineBool env = do
                line <- getCursorLine cm
                interpretCommandsLine cm lineBool line env

interpretCommandsLine :: JSObject -> Bool -> Int -> Env -> UI ()
interpretCommandsLine cm lineBool line env = do
    contentsControl <- liftUI $ getValue cm
    editorNum <- liftUI $ getEditorNumber cm
    out <- liftUI getOutputEl
    let bs = getBlocks contentsControl
        blockMaybe = if lineBool then getLineContent line (linesNum contentsControl) else getBlock line bs
    case blockMaybe of
        Nothing -> void $ liftUI $ element out # set UI.text "Failed to get Block"
        Just (Block blockLineStart blockLineEnd block) -> case parseWithPos editorNum (blockLineStart + 1) block of
                  Left err -> errorUI $ errorBundlePretty err
                  Right actions ->  do
                                asr <- liftIO $ sequence $ map (processAction env) actions
                                case resolveActionResponse (ASucc "") asr of
                                          AErr e -> errorUI e
                                          ASucc e -> successUI >> outputUI e
         where successUI = liftUI $ flashSuccess cm blockLineStart blockLineEnd
               errorUI err = (liftUI $ flashError cm blockLineStart blockLineEnd) >> (void $ liftUI $ element out # set UI.text err)
               outputUI o = void $ liftUI $ element out # set UI.text o

processAction :: Env -> Action -> IO ActionResponse
processAction env (Exec idd t) = do
                        putMVar (hintM env) $ MMini (compile $ simplify t)
                        res <- liftIO $ takeMVar (hintR env)
                        case res of
                          RMini m -> T.streamReplace (streamE env) idd m >> return (ASucc "")
                          RError e -> return $ AErr e
                          _ -> return $ AErr "Unkown error!"
processAction env (Show t) = do
                        putMVar (hintM env) $ MMini (compile $ simplify t)
                        res <- liftIO $ takeMVar (hintR env)
                        case res of
                          RMini m -> return (ASucc $ show m)
                          RError e -> return $ AErr e
                          _ -> return $ AErr "Unkown error!"
processAction env (Def t) = do
                        putMVar (hintM env) $ MDef (compileDef $ simplifyDef t)
                        res <- liftIO $ takeMVar (hintR env)
                        case res of
                          RSucc -> return (ASucc "")
                          RError e -> return $ AErr e
                          _ -> return $ AErr "Unkown error!"
processAction env (Type t) = do
                        putMVar (hintM env) $ MType (compile $ simplify t)
                        res <- liftIO $ takeMVar (hintR env)
                        case res of
                          RType typ -> return (ASucc typ)
                          RError e -> return $ AErr e
                          _ -> return $ AErr "Unkown error!"
processAction env (Load path) = do
           mayfile <- ((try $ readFile path) :: IO (Either SomeException String))
           case mayfile of
             Right file -> runManyDefs (getBlocks file)
             Left _ -> return $ AErr "Could not find the file!"
           where runManyDefs [] = return $ ASucc "Successfully loaded file!"
                 runManyDefs ((Block _ _ cont):ds) = do
                                     case parseDef 1 1 cont of
                                               Left err -> return $ AErr $ errorBundlePretty err
                                               Right ps -> do
                                                       res <- sequence $ map (\p -> (liftIO $ putMVar (hintM env) $ MDef (compileDefWithoutContext $ simplifyDef p)) >> (liftIO $ takeMVar (hintR env))) ps
                                                       case checkForErrs res of
                                                         RSucc -> runManyDefs ds
                                                         RError e -> return $ AErr e
                                                         _ -> return $ AErr "Unkown error!"
                                                       where checkForErrs [] = RSucc
                                                             checkForErrs (RSucc:xs) = checkForErrs xs
                                                             checkForErrs ((RError e):_) = RError e
                                                             checkForErrs _ = RError "Unknown error"


resolveActionResponse :: ActionResponse -> [ActionResponse] -> ActionResponse
resolveActionResponse curr [] = curr
resolveActionResponse _ ((AErr e):_) = AErr e
resolveActionResponse _ (a:as) = resolveActionResponse a as
