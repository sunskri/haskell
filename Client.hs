import Control.Concurrent
import Control.Monad
import Network
import System.IO

main :: IO ()
main = withSocketsDo $ do
    handle <- connectTo "127.0.0.1" (PortNumber 20000)
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
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
    putStrLn line
    receive handle



