module Editor.Backend where

import Control.Monad  (void)

import qualified Sound.Tidal.Context as T (Stream, streamReplace)

import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)

import Foreign.JavaScript (JSObject)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

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
    out <- liftUI getOutputEl
    let bs = getBlocks contentsControl
        blockMaybe = if lineBool then getLineContent line (linesNum contentsControl) else getBlock line bs
    case blockMaybe of
        Nothing -> void $ liftUI $ element out # set UI.text "Failed to get Block"
        Just (Block blockLineStart blockLineEnd block) ->  do
                                              -- evaluate the given expression, if a string is returned, print it to the console
                                              case parseTerm block of
                                                    Left err -> errorUI $ show err
                                                    Right t -> do
                                                            liftIO $ putStrLn $ show t
                                                            liftIO $ putStrLn $ (compile $ simplify t)
                                                            liftIO $ putMVar mMV $ (compile $ simplify t)
                                                            res <- liftIO $ takeMVar rMV
                                                            case res of
                                                              RMini m -> do
                                                                successUI
                                                                outputUI $ show m
                                                                liftIO $ T.streamReplace str 1 m
                                                              RError e -> errorUI e
         where successUI = liftUI $ flashSuccess cm blockLineStart blockLineEnd
               errorUI err = (liftUI $ flashError cm blockLineStart blockLineEnd) >> (void $ liftUI $ element out # set UI.text err)
               outputUI o = void $ liftUI $ element out # set UI.text o
