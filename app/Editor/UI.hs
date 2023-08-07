module Editor.UI where

import Control.Monad  (void)

import Sound.Tidal.Context hiding ((#))-- (Stream, sPMapMV, Pattern, queryArc, Arc(..))

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar  (modifyMVar_, MVar)

import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Map as Map  (empty)

import Foreign.JavaScript (JSObject)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text, value, get)

import Zwirn.Language.Compiler (Environment (..))

data Env = Env {windowE :: Window
               ,streamE :: Stream
               ,compilerE :: Environment
               ,hydraE :: MVar (Pattern String)
               }

hush :: Stream -> IO ()
hush str  = modifyMVar_ (sPMapMV str) (\_ -> return Map.empty)

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
getCursorLine cm = callFunction $ (ffi "getCursorLine(%1)") cm

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

addEvent :: Element -> UI ()
addEvent e = runFunction $ ffi code e
          where code = "(%1).addEventListener(\"keydown\",(e) => {const nevent = new CustomEvent(\"keyevent\", { detail: event.key }); if (event.ctrlKey){%1.dispatchEvent(nevent);}},false,);"
                codewrong = "{x = document.getElementsByTagName(\"BODY\")[0]; x.on('keydown', function(instance, event) {const nevent = new CustomEvent(\"keyevent\", { detail: event.key }); editor0.dispatchEvent(nevent);});}"

 -- adding and removing editors

makeEditor :: String -> UI ()
makeEditor i = runFunction $ ffi $ i ++ "cm = CodeMirror.fromTextArea(document.getElementById('" ++ i ++ "'), editorSettings);"

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
