module Editor.Backend where

{-
    Backend.hs - Implements the interaction between the compiler-interpreter and the editor
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

import Control.Monad  (void)

import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)

import Foreign.JavaScript (JSObject)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Editor.UI
import Zwirn.Language.Compiler

import Data.Text (pack)

data EvalMode
  = EvalBlock
  | EvalLine
  | EvalWhole
  deriving (Eq, Show)

evalContentAtCursor :: EvalMode -> JSObject -> MVar Environment -> UI ()
evalContentAtCursor mode cm envMV = do
                line <- getCursorLine cm
                evalContentAtLine mode cm line envMV

evalContentAtLine :: EvalMode -> JSObject -> Int -> MVar Environment -> UI ()
evalContentAtLine mode cm line envMV = do
                editorContent <- getValue cm
                editorNum <- getEditorNumber cm
                out <- getOutputEl
                env <- liftIO $ takeMVar envMV
                let ci = case mode of
                            EvalBlock -> compilerInterpreterBlock line editorNum (pack editorContent)
                            EvalLine -> compilerInterpreterLine line editorNum (pack editorContent)
                            EvalWhole -> compilerInterpreterWhole editorNum (pack editorContent)
                res <- liftIO $ runCI env ci
                case res of
                      Left (CIError err (Just (CurrentBlock st end))) -> do
                                          flashError cm st end
                                          void $ element out # set UI.text err  --TODO: get block start and end for flashing error
                                          liftIO $ putMVar envMV env
                      Left (CIError err Nothing) -> do
                                          void $ element out # set UI.text err  --TODO: get block start and end for flashing error
                                          liftIO $ putMVar envMV env
                      Right (resp, newEnv, st, end) -> do
                                          flashSuccess cm st end
                                          _ <- element out # set UI.text resp
                                          liftIO $ putMVar envMV newEnv
