import Control.Concurrent
import Control.Monad
import Network
import System.IO

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 20000
    return ()

{--
    let mvars = []
        handles = []
        loop [sock, mvars, handles]
        where loop args = forever $ do
                let
                    sock = args    !! 0
                    mvars = args   !! 1
                    handles = args !! 2
                    (handle, host, port) <- accept sock
                    nMvar <- getEmptyMVar handle
                    handleToMVars handle (mvars)
                    forkFinally (newConnection handle handles nMvar)
                    loop [sock, nMvar : mvars, handle : handles]
                    --}

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
    
    
    
    {--
    where loop handle hs mvar = do
            maybeHandle <- tryTakeMVar mvar
            case maybeHandle of
                Nothing -> do
                    line <- hGetLine handle
                    mapM_ (\h -> hPutStrLn h line) hs
                    loop $ handle hs
                _ -> do 
                    loop $ handle maybeHandle : hs 
    
    --}
                    
    
    
            
            
        
