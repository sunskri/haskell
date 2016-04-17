import Control.Concurrent
import Control.Monad
import Network
import System.IO

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 20000
    process (sock, [], [])


process :: (Socket, [MVar Handle], [Handle]) -> IO ()
process (s, ms, hs) = do
    (handle, host, port) <- accept s
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    nMvar <- getEmptyMVar handle
    forkIO $ handleToMVars handle ms
    forkFinally (newConnection handle hs nMvar) (\_ -> hClose handle)
    process(s, nMvar:ms, handle:hs)
    

getEmptyMVar :: Handle -> IO (MVar Handle)
getEmptyMVar handle = do
    m <- newMVar handle
    r <- takeMVar m
    return (m)

handleToMVars :: Handle -> [MVar Handle] -> IO ()
handleToMVars handle mvars = do
    mapM_ (\m -> putMVar m handle) mvars
  
            
newConnection :: Handle -> [Handle] -> MVar Handle -> IO ()
newConnection handle handles mvar = do
    maybeHandle <- tryTakeMVar mvar
    case maybeHandle of
        Nothing -> do
            line <- hGetLine handle
            mapM_ (\h -> hPutStrLn h line) handles
        Just h  -> do
            newConnection handle (h : handles) mvar    
   
    
    
            
            
        
