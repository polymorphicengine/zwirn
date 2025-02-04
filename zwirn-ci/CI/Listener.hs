module CI.Listener where

import Control.Monad.State (gets)
import Data.Bifunctor (first)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Network.Socket as N
import Sound.Osc as O
import Sound.Osc.Transport.Fd.Udp as O
import Zwirn.Language.Compiler
import Zwirn.Stream (sLocal)

type RemoteAddress = N.SockAddr

listenerStartMessage :: IO ()
listenerStartMessage = putStrLn "Starting Listener.. listening for messages on port 2323"

runListener :: Environment -> IO ()
runListener env = do
  eithenv <- runCI env listen
  case eithenv of
    Left (CIError err newEnv) -> print err >> runListener newEnv
    Right _ -> runListener env

listen :: CI ()
listen = recvMessageFrom >>= act >> listen

recvMessageFrom :: CI (Maybe Message, RemoteAddress)
recvMessageFrom = getUdp >>= \udp -> liftIO $ fmap (first packet_to_message) (recvFrom udp)

act :: (Maybe O.Message, RemoteAddress) -> CI ()
act (Just (Message "/ping" []), remote) = replyOK remote
act (Just (Message "/eval" [AsciiString stat]), remote) = compilerInterpreterBasic (T.pack (ascii_to_string stat)) >>= \s -> if null s then replyOK remote else replyOKVal remote s
act (Just m, remote) = replyError remote ("Unhandeled Message: " ++ show m)
act _ = return ()

reply :: RemoteAddress -> O.Packet -> CI ()
reply remote msg = getUdp >>= \local -> liftIO $ O.sendTo local msg remote

replyOK :: RemoteAddress -> CI ()
replyOK = flip reply (O.p_message "/ok" [])

replyOKVal :: RemoteAddress -> String -> CI ()
replyOKVal remote str = reply remote (O.p_message "/ok" [utf8String str])

replyError :: RemoteAddress -> String -> CI ()
replyError remote err = reply remote (O.p_message "/error" [utf8String err])

replyErrorEnv :: Environment -> RemoteAddress -> String -> IO ()
replyErrorEnv env remote err = O.sendTo udp msg remote
  where
    msg = O.p_message "/error" [utf8String err]
    udp = sLocal $ tStream env

utf8String :: String -> O.Datum
utf8String s = O.AsciiString $ encodeUtf8 $ T.pack s

toUTF8 :: O.Ascii -> String
toUTF8 x = T.unpack $ decodeUtf8 x

getUdp :: CI O.Udp
getUdp = gets (sLocal . tStream)
