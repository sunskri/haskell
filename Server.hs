import Control.Concurrent
import Control.Monad
import Network

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 20000
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
            forFinally (newConnection handles nMvar)
            loop [sock, nMvar : mvars, handle : handles]

getEmptyMVar :: Handle -> IO (MVar)
getEmptyMVar handle = do
    m <- newMVar handle
    r <- takeMVar m
    return (m)

handleToMVars :: Handle -> [MVar] -> IO ()
handleToMVars handle mvars = do
    mapM_ (\m -> putMVar m handle) mvars
            
newConnection :: [Handle] -> MVar -> IO ()
newConnection hs mvar = forever $ do
    
            
            
        
