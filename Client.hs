import Control.Concurrent
import Control.Monad
import Network
import System.IO (hSetNewlineMode, universalNewlineMode, hSetBuffering, hPutStrLn, hGetLine, Handle, BufferMode(NoBuffering), hSetEncoding, TextEncoding, utf8)
import Prelude hiding (putStrLn)
import Data.ByteString.Char8 (putStrLn)
import Data.ByteString.UTF8 (fromString)

main :: IO ()
main = withSocketsDo $ do
    handle <- connectTo "127.0.0.1" (PortNumber 20666)
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle NoBuffering
    hSetEncoding handle utf8
    putStrLn $ fromString "Enter your name / Введите ваше имя :"
    username <- getLine
    hPutStrLn handle username
    forkIO $ receive handle
    transmitt handle

    
transmitt :: Handle -> IO ()
transmitt handle = do
    line <- getLine
    hPutStrLn handle line
    transmitt handle
    
receive :: Handle -> IO ()
receive handle = do
    line <- hGetLine handle
    putStrLn $ fromString line
    receive handle



