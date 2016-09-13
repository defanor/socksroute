{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.Socks5.Lowlevel
import Network.Socks5.Types
import Network.Socks5
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import qualified Network.Simple.TCP as NST
import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Control.Exception as E


data Rule = Socks SocksConf
          | Plain

domainRules :: SocksHostAddress -> Rule
domainRules (SocksAddrDomainName s)
  | BS.isSuffixOf ".onion" s = tor
  | BS.isSuffixOf "archive.org" s = tor
  | BS.isSuffixOf "trulyergonomic.com" s = tor
  | BS.isSuffixOf "googlevideo.com" s = Plain
  | BS.isInfixOf "google" s = tor
  | BS.isSuffixOf "gstatic.com" s = tor
  | BS.isSuffixOf "rutracker.org" s = tor
  | BS.isSuffixOf "thepiratebay.se" s = tor
  | BS.isSuffixOf "wordpress.com" s = tor
  | otherwise = Plain
  where
    tor = Socks $ defaultSocksConf "localhost" $ fromIntegral 9050
domainRules _ = Plain


toSockAddr :: PortNumber -> SocksHostAddress -> IO (Maybe SockAddr)
toSockAddr pn (SocksAddrIPV4 ha4) = return . Just $ SockAddrInet pn ha4
toSockAddr pn (SocksAddrIPV6 ha6) = return . Just $ SockAddrInet6 pn 0 ha6 0
toSockAddr pn (SocksAddrDomainName dn) = do
  -- Just AF_INET here, no AF_INET6; that would require to also return
  -- a family. Easy to add, but I don't need (and don't have) IPv6.
  ai <- getAddrInfo (Just $ defaultHints { addrSocketType = Stream,
                                           addrFamily = AF_INET })
        (Just $ BS.unpack dn) (Just $ show pn)
  case ai of
    -- Would be better to return the whole list, and try them in
    -- order; ToDo, if will try to make it all nice someday.
    (x:_) -> return . Just $ addrAddress x
    _ -> return Nothing


main :: IO ()
main = NST.serve (NST.Host "127.0.0.1") "1080" $ \(s, saddr) -> do
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
      c2s <- async $ relay f t
      s2c <- async $ relay t f
      (a, r) <- waitAnyCatchCancel [c2s, s2c]
      case r of
        Left e -> putStrLn $ "Exception: " ++ show e
        Right v -> return ()
      tc <- isConnected t
      when tc $ close t
      fc <- isConnected f
      when fc $ close f

    relay :: Socket -> Socket -> IO ()
    relay f t = do
      v <- NSB.recv f 4096
      case v of
        "" -> return ()
        v' -> NSB.sendAll t v' >> relay f t
