-- | Extremely minimal HTTP server.
module Nyanbda.Web.HttpServer
  ( module HTTP
  , module URI
  , PortNumber
  , serve, respond, notFound
  ) where
import Control.Concurrent
import Control.Exception
import Network.Socket as S
import Network.HTTP as HTTP
import Network.URI as URI

-- | The simplest possible HTTP server: accept requests on the given port,
--   handle each request using the given handler.
serve :: HStream t => PortNumber -> ((Request t) -> IO (Response t)) -> IO ()
serve port handle = do
  bracket (socket AF_INET Stream defaultProtocol) S.close $ \sock -> do
    bind sock (SockAddrInet port 0)
    listen sock 5
    putStrLn $ "Listening on port " ++ show port
    forever $ do
      (sock', addr) <- accept sock
      forkIO $ bracket (socketConnection (getHost addr) (getPort addr) sock')
                       HTTP.close
                       runHandler
  where
    getHost = takeWhile (/= ':') . show
    getPort (SockAddrInet port _) = fromIntegral port
    forever m = m >> forever m
    runHandler s = do
      res <- receiveHTTP s
      case res of
        Left _    -> return ()
        Right req -> handle req >>= respondHTTP s

-- | A successful response.
respond :: HStream t => t -> IO (Response t)
respond x = return Response
  { rspCode = (2,0,0)
  , rspReason = ""
  , rspHeaders = []
  , rspBody = x
  }

-- | A 404 error.
notFound :: HStream t => t -> IO (Response t)
notFound x = return Response
  { rspCode = (4,0,4)
  , rspReason = ""
  , rspHeaders = []
  , rspBody = x
  }
