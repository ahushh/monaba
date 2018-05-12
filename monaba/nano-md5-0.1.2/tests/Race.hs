import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when, forever)
import Data.Digest.OpenSSL.MD5 as N -- nano (OpenSSL Binding)
import Data.Digest.MD5 as P -- Pure (only Haskell)
import Data.ByteString as B
import Data.ByteString.Lazy as L

main = do
    let bs1 = B.pack [1..10]
        answer1 = show $ P.md5 $ fromChunks [bs1]
        bs2 = B.pack [11..20]
        answer2 = show $ P.md5 $ fromChunks [bs2]
        bs3 = B.pack [1..255]
        answer3 = show $ P.md5 $ fromChunks [bs3]
        assert = confirm "assert: "
        race = \x -> forever (confirm "FAILURE: " x)
    assert (return bs1, answer1)
    assert (return bs2, answer2)
    assert (return bs3, answer3)
    forkIO $ race (return bs1, answer1)
    forkIO $ race (return bs2, answer2)
    forkIO $ race (return bs3, answer3)
    threadDelay maxBound

confirm :: String -> (IO B.ByteString, String) -> IO ()
confirm prefix (getBS,str) = do
    bs <- getBS
    let s = N.md5sum bs
    when (s /= str) (error (prefix ++ s ++ " does not equal " ++ str))
