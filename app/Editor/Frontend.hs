module Editor.Frontend where

{-
    Frontend.hs - defines the html dom for the editor interface
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

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Control.Monad  (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Editor.Hydra

frontend :: Window -> UI Element
frontend win = do
  void $ return win # set title "zwirn"

  UI.addStyleSheet win "tidal.css"
  UI.addStyleSheet win "theme.css"

  setCallBufferMode NoBuffering -- important for highlighting

  mainEditor <- UI.div #. "main"
                       #+ [UI.textarea # set UI.id_ "editor0"]
                       # set UI.style [("flex-grow","8")]

  container  <- UI.div # set UI.id_ "container"
                       #. "flex-container CodeMirror cm-s-tomorrow-night-eighties"

  editorContainer <- UI.div # set UI.id_ "editors"
                            #. "flex-container"
                            #+ [element mainEditor]
                            # set UI.style [("display","flex"),("flex-wrap","wrap")]

  body <- UI.getBody win # set UI.style [("background-color","black")]

  void $ (element body) #+
                    [canvas
                    ,element container #+ [ element editorContainer
                                          , outputWrapper]
                    ]
  return mainEditor

tidalSettings :: UI Element
tidalSettings = do
          execPath <- liftIO $ dropFileName <$> getExecutablePath
          tidalKeys <- liftIO $ readFile $ execPath ++ "static/tidalConfig.js"
          settings <- mkElement "script" # set UI.text tidalKeys
          return settings

canvas :: UI Element
canvas = do
  winWidth <- getWindowWidth
  winHeight <- getWindowHeight
  UI.canvas # set UI.id_ "hydraCanvas" # set style [("position", "fixed")
                                                   ,("left","0")
                                                   ,("top","0")
                                                   ,("width","100%")
                                                   ,("height","100%")
                                                   ,("pointer-events","none")]
                                       # set UI.width (round $ winWidth*2)
                                       # set UI.height (round $ winHeight*2)

outputWrapper:: UI Element
outputWrapper =  UI.div #+ [ UI.pre # set UI.id_ "output"
                                   #. "outputBox"
                                   # set style [("font-size","3vh")]
                          ]

fileInput :: UI Element
fileInput = UI.input # set UI.id_ "fileInput"
                       # set UI.type_ "file"
                       # set style [("display","none")]
