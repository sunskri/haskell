import Control.Concurrent
import Control.Monad
import Network
import System.IO
import GHC.Conc.Sync
import Data.IORef

data Connection = Connection User Handle

getUser :: Connection -> User
getUser (Connection user _) = user

getHandle :: Connection -> Handle
getHandle (Connection _ handle) = handle

data User = User String deriving (Show, Eq)

getUsername :: User -> String
getUsername (User name) = name


main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 20666
    ir <- newIORef []
    mv <- newMVar 0
    process (sock, ir, mv)


process :: (Socket, IORef [Connection], MVar Int) -> IO ()
process (s, ir, mv) = do
    (handle, host, port) <- accept s
    putStrLn $ "New connection " ++ host
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    forkFinally (newConnection handle ir (User "") mv) (\_ -> hClose handle)
    process (s, ir, mv)
   
        

newConnection :: Handle -> IORef [Connection] -> User -> MVar Int -> IO ()
newConnection handle ir u mv = do
    line <- hGetLine handle
    let user = User line
    cons <- readIORef ir
    atomicWriteIORef ir $ (Connection user handle) : cons
    serveConnection handle ir user mv

    
serveConnection :: Handle -> IORef [Connection] -> User -> MVar Int -> IO ()    
serveConnection handle ir user mv = do
    cons <- readIORef ir
    line <- hGetLine handle
    loop line cons
    where loop line cons = do
            v <- tryTakeMVar mv
            case v of
                Nothing -> do loop line cons
                _       -> do
                    --fcons <- filterConnections (filter (\c -> getUser c /= user) cons) []
                    mapM_ (\h -> hPutStrLn h (getUsername user ++ ": " ++ line)) (map getHandle (filter (\c -> getUser c /= user) cons))
                    putMVar mv 0
                    serveConnection handle ir user mv
    

filterConnections :: [Connection] -> [Connection] -> IO ([Connection])
filterConnections [] fcs = do return (fcs)
filterConnections (c:cs) fcs = do
    open <- hIsOpen $ getHandle c
    if open then filterConnections cs (c:fcs)
    else filterConnections cs fcs
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
