module Editor.Backend where

import Control.Monad  (void)

import qualified Sound.Tidal.Context as T (streamReplace)
import Sound.Tidal.ID (ID (..))

import Control.Concurrent.MVar  (putMVar, takeMVar, modifyMVar_)
import Control.Exception (try, SomeException)

import Foreign.JavaScript (JSObject)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

-- import Editor.Block
import Editor.UI
import Zwirn.Language
import Zwirn.Language.Hint

import Data.Text (pack)

interpretCommands :: JSObject -> Bool -> Env -> UI ()
interpretCommands cm lineBool env = do
                line <- getCursorLine cm
                interpretCommandsLine cm lineBool line env

interpretCommandsLine :: JSObject -> Bool -> Int -> Env -> UI ()
interpretCommandsLine cm lineBool line env = do
                editorContent <- liftUI $ getValue cm
                editorNum <- liftUI $ getEditorNumber cm
                out <- liftUI getOutputEl
                res <- liftIO $ runCI (compilerE env) (compilerInterpreter line editorNum (pack editorContent))
                case res of
                      Left err -> void $ liftUI $ element out # set UI.text err  --TODO: get block start and end for flashing error
                      Right (resp, newEnv, st, end) -> do
                                          liftUI $ flashSuccess cm st end
                                          _ <- liftUI $ element out # set UI.text resp
                                          return ()
