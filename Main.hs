{-# LANGUAGE OverloadedStrings #-}

import Network.Socks5.Lowlevel
import Network.Socks5.Types
import Network.Socks5
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString as BS
import Control.Exception as E
import Control.Concurrent
import Control.Applicative
import Data.Char

data Rule = Socks SocksConf
          | Plain

domainRules :: SocksHostAddress -> Rule
domainRules (SocksAddrDomainName s)
  | BS.isSuffixOf ".onion" s = tor
  | otherwise = Plain
  where
    tor = Socks $ defaultSocksConf
      (SockAddrInet (fromIntegral 9050) (tupleToHostAddress (127, 0, 0, 1)))
domainRules _ = Plain

splitStrings :: [BS.ByteString]
splitStrings = ["googlevideo.com"]

toSockAddr :: PortNumber -> SocksHostAddress -> IO (Maybe SockAddr)
toSockAddr pn (SocksAddrIPV4 ha4) = return . Just $ SockAddrInet pn ha4
toSockAddr pn (SocksAddrIPV6 ha6) = return . Just $ SockAddrInet6 pn 0 ha6 0
toSockAddr pn (SocksAddrDomainName dn) = do
  -- Just AF_INET here, no AF_INET6; that would require to also return
  -- a family. Easy to add, but I don't need (and don't have) IPv6.
  ai <- getAddrInfo (Just $ defaultHints { addrSocketType = Stream,
                                           addrFamily = AF_INET })
        (Just $ map (chr . fromIntegral) $ BS.unpack dn) (Just $ show pn)
  case ai of
    -- Would be better to return the whole list, and try them in
    -- order; ToDo, if will try to make it all nice someday.
    (x:_) -> return . Just $ addrAddress x
    _ -> return Nothing

serveTCP :: (Socket -> SockAddr -> IO ()) -> IO ()
serveTCP f = do
  let addr = SockAddrInet (fromIntegral 1080) (tupleToHostAddress (127, 0, 0, 1))
  E.bracket
    (socket AF_INET Stream defaultProtocol)
    close
    $ \sock -> do
    setSocketOption sock ReuseAddr 1
    setSocketOption sock NoDelay 1
    bind sock addr
    withFdSocket sock setCloseOnExecIfNeeded
    listen sock 128
    forever $ do
      (conn, peer) <- accept sock
      forkIO (f conn peer `finally` close conn)

main :: IO ()
main = serveTCP $ \s saddr -> do
  (SocksRequest cmd sdAddr sdPort) <- socksListen s
  case cmd of
    SocksCommandConnect -> do
      case domainRules sdAddr of
        Plain -> do
          msa <- toSockAddr sdPort sdAddr
          case msa of
            Nothing -> putStrLn "Failed to get sockaddr"
            Just sa -> do
              s' <- socket AF_INET Stream defaultProtocol
              setSocketOption s' NoDelay 1
              connect s' sa
              sendSerialized s $ SocksResponse SocksReplySuccess sdAddr sdPort
              proxy s s'
        Socks sc -> do
          (s', (sha, pn)) <- socksConnect sc (SocksAddress sdAddr sdPort)
          sendSerialized s $ SocksResponse SocksReplySuccess sha pn
          proxy s s'
    _ -> putStrLn "Unknown SOCKS command"
  where
    proxy :: Socket -> Socket -> IO ()
    proxy f t = do
      -- Might be better to use `poll` here, not separate threads.
      c2s <- async $ relay f t splitStrings
      s2c <- async $ relay t f []
      (a, r) <- waitAnyCatchCancel [c2s, s2c]
      case r of
        Left e -> putStrLn $ "Exception: " ++ show e
        Right v -> return ()
      close t
      close f

    tryToBreak :: BS.ByteString -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
    tryToBreak whole sub = case BS.breakSubstring sub whole of
      (x, "") -> Nothing
      (x, y') -> case BS.splitAt (BS.length sub `div` 2) y' of
        (y1, y2) -> pure (BS.append x y1, y2)

    -- A very crude TLS record splitting
    tlsTryToBreak whole sub =
      let (s1, s2) = BS.splitAt 5 whole
      in case BS.unpack s1 of
        [22, v1, v2, l1, l2] ->
          let len = fromIntegral l1 * 0x100 + fromIntegral l2
          in do
            (b1, b2) <- tryToBreak s2 sub
            let l1 = BS.length b1
                l2 = BS.length b2
                mkHeader l = BS.pack [22, v1, v2,
                                      fromIntegral (l `div` 0x100),
                                      fromIntegral (l `mod` 0x100)]
            pure (BS.append (mkHeader l1) b1, BS.append (mkHeader l2) b2)
        _ -> tryToBreak whole sub

    relay :: Socket -> Socket -> [BS.ByteString] -> IO ()
    relay f t ss = do
      v <- NSB.recv f 4096
      case v of
        "" -> return ()
        v' -> do
          case foldl (<|>) Nothing (map (tlsTryToBreak v') splitStrings) of
            Nothing -> NSB.sendAll t v'
            Just (v1, v2) ->
              -- Might be useful to send those 200 ms or so apart, to
              -- avoid joining of the pockets. But the observed DPI
              -- does not seem to be confused by split TCP packets
              -- anyway, so depending more on the split TLS records
              -- here.
              NSB.sendAll t v1 >> NSB.sendAll t v2
          relay f t ss
