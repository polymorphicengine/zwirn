module Editor.Backend where

import Control.Monad  (void)

import qualified Sound.Tidal.Context as T (Stream, streamReplace)

import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)

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


interpretCommands :: JSObject -> Bool -> Env -> UI ()
interpretCommands cm lineBool env = do
                line <- getCursorLine cm
                interpretCommandsLine cm lineBool line env

interpretCommandsLine :: JSObject -> Bool -> Int -> Env -> UI ()
interpretCommandsLine cm lineBool line env = do
    let str = streamE env
        mMV = hintM env
        rMV = hintR env
    contentsControl <- liftUI $ getValue cm
    editorNum <- liftUI $ getEditorNumber cm
    out <- liftUI getOutputEl
    let bs = getBlocks contentsControl
        blockMaybe = if lineBool then getLineContent line (linesNum contentsControl) else getBlock line bs
    case blockMaybe of
        Nothing -> void $ liftUI $ element out # set UI.text "Failed to get Block"
        Just (Block blockLineStart blockLineEnd block) ->  do
                                              case parseWithPos editorNum (blockLineStart + 1) block of
                                                -- evaluate the given expression, if a string is returned, print it to the console
                                                Left err -> errorUI $ errorBundlePretty err
                                                Right (Exec idd t) -> do
                                                            liftIO $ putMVar mMV $ MMini (compile $ simplify t)
                                                            res <- liftIO $ takeMVar rMV
                                                            case res of
                                                              RMini m -> do
                                                                successUI
                                                                liftIO $ T.streamReplace str idd m
                                                              RError e -> errorUI e
                                                              _ -> errorUI "Unknown error!"
                                                Right (Show t) -> do
                                                            liftIO $ putMVar mMV $ MMini (compile $ simplify t)
                                                            res <- liftIO $ takeMVar rMV
                                                            case res of
                                                              RMini m -> do
                                                                successUI
                                                                outputUI $ show m
                                                              RError e -> errorUI e
                                                              _ -> errorUI "Unknown error!"
                                                Right (Def def) -> do
                                                             liftIO $ putMVar mMV $ MDef (compileDef $ simplifyDef def)
                                                             res <- liftIO $ takeMVar rMV
                                                             case res of
                                                               RSucc -> successUI >> outputUI ""
                                                               RError e -> errorUI e
                                                               _ -> errorUI "Unknown error!"
                                                Right (Type t) -> do
                                                            liftIO $ putMVar mMV $ MType (compile $ simplify t)
                                                            res <- liftIO $ takeMVar rMV
                                                            case res of
                                                              RType typ -> do
                                                                successUI
                                                                outputUI $ typ
                                                              RError e -> errorUI e
                                                              _ -> errorUI "Unknown error!"
         where successUI = liftUI $ flashSuccess cm blockLineStart blockLineEnd
               errorUI err = (liftUI $ flashError cm blockLineStart blockLineEnd) >> (void $ liftUI $ element out # set UI.text err)
               outputUI o = void $ liftUI $ element out # set UI.text o
