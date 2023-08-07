module Editor.Backend where

import Control.Monad  (void)

import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)

import Foreign.JavaScript (JSObject)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Editor.UI
import Zwirn.Language.Compiler

import Data.Text (pack)

interpretCommands :: JSObject -> Bool -> MVar Environment -> UI ()
interpretCommands cm lineBool envMV = do
                line <- getCursorLine cm
                interpretCommandsLine cm lineBool line envMV

interpretCommandsLine :: JSObject -> Bool -> Int -> MVar Environment -> UI ()
interpretCommandsLine cm lineBool line envMV = do
                editorContent <- getValue cm
                editorNum <- getEditorNumber cm
                out <- getOutputEl
                env <- liftIO $ takeMVar envMV
                res <- liftIO $ runCI env (compilerInterpreter line editorNum (pack editorContent))
                case res of
                      Left err -> void $ element out # set UI.text err  --TODO: get block start and end for flashing error
                      Right (resp, newEnv, st, end) -> do
                                          flashSuccess cm st end
                                          _ <- element out # set UI.text resp
                                          liftIO $ putMVar envMV newEnv
