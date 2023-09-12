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

import Control.Monad  (void)

import Sound.Tidal.Context hiding ((#))-- (Stream, sPMapMV, Pattern, queryArc, Arc(..))
import Sound.Tidal.Config as Conf

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar  (MVar, modifyMVar_)

import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Map as Map  (empty)
import Data.Text (Text, unpack, pack)

import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)

import Foreign.JavaScript (JSObject)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text, value, get)

import qualified Zwirn.Interactive.Types as Z (Text (..))

hush :: Stream -> (MVar (Pattern Z.Text)) -> IO ()
hush str hyd = do
  modifyMVar_ (sPMapMV str) (\_ -> return Map.empty)
  modifyMVar_ hyd (const $ pure $ pure $ Z.Text $ pack "solid().out()")

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

getCursorLine :: ToJS a => a -> UI Int
getCursorLine cm = callFunction $ ffi (wrapCatchErr "getCursorLine(%1)") cm

getValue :: ToJS a => a -> UI String
getValue cm = callFunction $ ffi "getV(%1)" cm

getEditorNumber :: ToJS a => a -> UI Int
getEditorNumber cm = do
   idd <- callFunction $ ffi "(%1).getTextArea().id" cm
   case idd of
     'e':'d':'i':'t':'o':'r':xs -> return $ read xs
     _ -> return 0

createHaskellFunction name fn = do
 handler <- ffiExport fn
 runFunction $ ffi ("window." ++ name ++ " = %1") handler

 -- adding and removing editors

catchJSErrors :: UI ()
catchJSErrors = runFunction $ ffi "window.onerror = function(msg, url, linenumber) { alert(msg);return true;}"

makeEditor :: String -> UI ()
makeEditor i = runFunction $ ffi $ i ++ "cm = CodeMirror.fromTextArea(document.getElementById('" ++ i ++ "'), fullSettings.editor);"

addEditor :: IORef [Element]  -> UI ()
addEditor ref = do
       old <- liftIO $ readIORef ref
       let x = show $ length old
       editor <- UI.textarea # set (attr "id") ("editor" ++ x)
       d <- UI.div #. "main" #+ [element editor] # set UI.style [("flex-grow","8")]
       liftIO $ modifyIORef ref (\xs -> xs ++ [d])
       redoEditorLayout ref
       makeEditor ("editor" ++ x)

removeEditor :: IORef [Element] -> UI ()
removeEditor ref = do
         xs <- liftIO $ readIORef ref
         case length xs == 1 of
           True -> return ()
           False -> do
               liftIO $ modifyIORef ref (\ys -> take (length xs - 1) ys)
               redoEditorLayout ref

redoEditorLayout :: IORef [Element] -> UI ()
redoEditorLayout ref = do
           win <- askWindow
           eds <- liftIO $ readIORef ref
           editorsMay <- getElementById win "editors"
           case editorsMay of
             Nothing -> error "cant happen"
             Just editors -> void $ element editors # set UI.children eds


-- flashing

checkUndefined :: ToJS a => a -> UI String
checkUndefined cm = callFunction $ ffi "(function (a) { if (typeof a === 'undefined' || a === null) {return \"yes\";} else { return \"no\"; } })(%1)" cm

highlightBlock :: JSObject -> Int -> Int -> String -> UI JSObject
highlightBlock cm lineStart lineEnd color = do
                                       undef <- checkUndefined cm
                                       case undef of
                                         "no" -> callFunction $ ffi "((%1).markText({line: %2, ch: 0}, {line: %3, ch: 0}, {css: %4}))" cm lineStart lineEnd color
                                         _ -> callFunction $ ffi "return {}"

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

flashSuccess :: JSObject -> Int -> Int -> UI ()
flashSuccess cm lineStart lineEnd = do
                           mark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: green"
                           liftIO $ threadDelay 100000
                           unHighlight mark
                           flushCallBuffer

flashError :: JSObject -> Int -> Int -> UI ()
flashError cm lineStart lineEnd = do
                           mark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: red"
                           liftIO $ threadDelay 100000
                           unHighlight mark
                           flushCallBuffer

-- setting, getting and clearing the config

setConfig :: Window -> Text -> Text -> IO ()
setConfig win key v = runUI win $ runFunction $ ffi ("window.electronAPI.putInStore(%1," ++ (unpack v) ++ ")") (unpack key)

clearConfig :: Window -> IO ()
clearConfig win = runUI win $ runFunction $ ffi "window.electronAPI.clearStore()"

configureTarget :: UI Target
configureTarget = do
              dirtport <- callFunction $ ffi "fullSettings.tidal.dirtport"
              latency <- callFunction $ ffi "fullSettings.tidal.latency"
              return $ superdirtTarget {oLatency = latency, oAddress = "127.0.0.1", oPort = dirtport}

configureStream :: UI Conf.Config
configureStream = do
  frameTimespan <- callFunction $ ffi "fullSettings.tidal.frameTimespan"
  processAhead <- callFunction $ ffi "fullSettings.tidal.processAhead"
  link  <- callFunction $ ffi "fullSettings.tidal.link"
  let linkB = case link of
                  "false" -> False
                  _ -> True
  skipTicks  <- callFunction $ ffi "fullSettings.tidal.skipTicks"
  quantum <- callFunction $ ffi "fullSettings.tidal.quantum"
  beatsPerCycle <- callFunction $ ffi "fullSettings.tidal.beatsPerCycle"
  return $ Conf.defaultConfig { cVerbose = False
                         , cFrameTimespan = frameTimespan
                         , cEnableLink = linkB
                         , cProcessAhead = processAhead
                         , cSkipTicks = read skipTicks
                         , cQuantum = read quantum
                         , cBeatsPerCycle = read beatsPerCycle
                         }

getBootPaths :: UI (Maybe [Text])
getBootPaths = do
        p <- callFunction $ ffi "fullSettings.bootPath"
        b <- liftIO $ doesDirectoryExist p
        case b of
          False -> do
            bb <- liftIO $ doesFileExist p
            case bb of
               False -> (getOutputEl # set UI.text (show p)) >> return Nothing
               True -> return $ Just [pack p]
          True -> fmap (\xs -> Just $ map (\x -> pack $ p ++ "/" ++ x) xs) $ liftIO $ listDirectory $ p

getHighlight :: UI Bool
getHighlight = do
      h <- callFunction $ ffi "fullSettings.highlight"
      case h of
        "true" -> return True
        _ -> return False

getHydra :: UI Bool
getHydra = do
      h <- callFunction $ ffi "fullSettings.hydra"
      case h of
        "true" -> return True
        _ -> return False

wrapCatchErr :: String -> String
wrapCatchErr st = "try {" ++ st ++ "} catch (err) {}"
