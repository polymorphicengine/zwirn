module Editor.Frontend where

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Control.Monad  (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (newMVar)

import Sound.Tidal.Context (Stream)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Editor.Setup
import Editor.UI
import Editor.Highlight



setup :: Stream -> Window -> UI ()
setup str win = void $ do
     --setup GUI
     void $ return win # set title "minilambda"

     UI.addStyleSheet win "tidal.css"
     UI.addStyleSheet win "theme.css"

     setCallBufferMode NoBuffering -- important for highlighting

     editor <- UI.textarea # set (attr "id") "editor0"

     output <- UI.pre # set UI.id_ "output"
                      #. "outputBox"
                      # set style [("font-size","3vh")]

     outputWrapper <- UI.div #+ [ element output]

     fileInput <- UI.input # set UI.id_ "fileInput"
                           # set UI.type_ "file"
                           # set style [("display","none")]

     mainEditor <- UI.div #. "main" #+ [element editor] # set UI.style [("flex-grow","8")]
     container <- UI.div # set UI.id_ "container" #. "flex-container CodeMirror cm-s-tomorrow-night-eighties"
     editorContainer <- UI.div # set UI.id_ "editors" #. "flex-container" #+ [element mainEditor] # set UI.style [("display","flex"),("flex-wrap","wrap")]

     body <- UI.getBody win  # set UI.style [("background-color","black")]

     createShortcutFunctions str mainEditor

     _ <- (element body) #+
                       [element container #+ [element editorContainer
                                             , element outputWrapper]
                       ]

     setupBackend str

     _ <- (element body) #+
                        [element fileInput
                        ,tidalSettings
                        ]
     makeEditor "editor0"

     buf <- liftIO $ newMVar []
     liftIO $ forkIO $ highlightLoopOuter win str buf


tidalSettings :: UI Element
tidalSettings = do
          execPath <- liftIO $ dropFileName <$> getExecutablePath
          tidalKeys <- liftIO $ readFile $ execPath ++ "static/tidalConfig.js"
          settings <- mkElement "script" # set UI.text tidalKeys
          return settings
