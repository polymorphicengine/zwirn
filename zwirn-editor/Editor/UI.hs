{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Editor.UI where

{-
    UI.hs - miscellanious functions for the user interface
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

-- (Stream, sPMapMV, Pattern, queryArc, Arc(..))
-- import Sound.Tidal.Config as Conf

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (unless, void)
import Control.Monad.Catch (catch)
import Data.IORef (IORef, modifyIORef, readIORef)
import Foreign.JavaScript (JSObject)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (get, text, value)
import Zwirn.Stream

hush :: Stream -> x -> IO ()
hush _ _ = return ()

getOutputEl :: UI Element
getOutputEl = do
  win <- askWindow
  elMay <- getElementById win "output"
  case elMay of
    Nothing -> error "can't happen"
    Just el -> return el

getDisplayElV :: UI Element
getDisplayElV = do
  win <- askWindow
  elMay <- getElementById win "displayV"
  case elMay of
    Nothing -> error "can't happen"
    Just el -> return el

getCursorLine :: (ToJS a) => a -> UI Int
getCursorLine cm = catchHaskellError 0 $ callFunction $ ffi "getCursorLine(%1)" cm

getValue :: (ToJS a) => a -> UI String
getValue cm = catchHaskellError "" $ callFunction $ ffi "getV(%1)" cm

getEditorNumber :: (ToJS a) => a -> UI Int
getEditorNumber cm = do
  idd <- catchHaskellError "editor0" $ callFunction $ ffi "(%1).getTextArea().id" cm
  case idd of
    'e' : 'd' : 'i' : 't' : 'o' : 'r' : xs -> return $ read xs
    _ -> return 0

createHaskellFunction name fn = do
  handler <- ffiExport fn
  runFunction $ ffi ("window." ++ name ++ " = %1") handler

-- adding and removing editors

makeEditor :: String -> UI ()
makeEditor i = runFunction $ ffi $ i ++ "cm = CodeMirror.fromTextArea(document.getElementById('" ++ i ++ "'), editorSettings);"

addEditor :: IORef [Element] -> UI ()
addEditor ref = do
  old <- liftIO $ readIORef ref
  let x = show $ length old
  editor <- UI.textarea # set (attr "id") ("editor" ++ x)
  d <- UI.div #. "main" #+ [element editor] # set UI.style [("flex-grow", "8")]
  liftIO $ modifyIORef ref (\xs -> xs ++ [d])
  redoEditorLayout ref
  makeEditor ("editor" ++ x)

removeEditor :: IORef [Element] -> UI ()
removeEditor ref = do
  xs <- liftIO $ readIORef ref
  unless
    (length xs == 1)
    ( do
        liftIO $ modifyIORef ref (take (length xs - 1))
        redoEditorLayout ref
    )

redoEditorLayout :: IORef [Element] -> UI ()
redoEditorLayout ref = do
  win <- askWindow
  eds <- liftIO $ readIORef ref
  editorsMay <- getElementById win "editors"
  case editorsMay of
    Nothing -> error "cant happen"
    Just editors -> void $ element editors # set UI.children eds

-- flashing

highlightBlock :: JSObject -> Int -> Int -> String -> UI (Maybe JSObject)
highlightBlock cm lineStart lineEnd color = catchHaskellErrorMaybe $ callFunction $ ffi "((%1).markText({line: %2, ch: 0}, {line: %3, ch: 0}, {css: %4}))" cm lineStart lineEnd color

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

flashSuccess :: JSObject -> Int -> Int -> UI ()
flashSuccess cm lineStart lineEnd = do
  maymark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: green"
  case maymark of
    Just mark -> do
      liftIO $ threadDelay 100000
      unHighlight mark
      flushCallBuffer
    Nothing -> return ()

flashError :: JSObject -> Int -> Int -> UI ()
flashError cm lineStart lineEnd = do
  maymark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: red"
  case maymark of
    Just mark -> do
      liftIO $ threadDelay 100000
      unHighlight mark
      flushCallBuffer
    Nothing -> return ()

addMessage :: String -> UI ()
addMessage s = getOutputEl >>= \out -> void $ element out # set UI.text s

-- to catch javascript errors when using callFunction
catchHaskellError :: a -> UI a -> UI a
catchHaskellError x action = catch action (\(e :: SomeException) -> addMessage (show e) >> return x)

catchHaskellErrorMaybe :: UI a -> UI (Maybe a)
catchHaskellErrorMaybe action = catch (fmap Just action) (\(e :: SomeException) -> addMessage (show e) >> return Nothing)
