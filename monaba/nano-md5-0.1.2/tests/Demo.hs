    import System.Environment
    import Data.Digest.OpenSSL.MD5
    import qualified Data.ByteString as B

    main = do
        [f] <- getArgs
        putStrLn . md5sum =<< B.readFile f
