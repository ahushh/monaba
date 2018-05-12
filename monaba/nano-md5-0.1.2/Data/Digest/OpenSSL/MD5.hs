{-# OPTIONS -#include "openssl/md5.h" #-}

--------------------------------------------------------------------
-- |
-- Module    :  Data.Digest.OpenSSL.MD5
-- Copyright :  (c) Galois, Inc. 2007
-- License   :  BSD3
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: Requires FFI
--
--------------------------------------------------------------------
--
-- ByteString-based, zero-copying binding to OpenSSL's md5 interface
--

module Data.Digest.OpenSSL.MD5 where


--
-- A few imports, should tidy these up one day.
--
#if __GLASGOW_HASKELL__ >= 608
import qualified Data.ByteString.Unsafe as B (unsafeUseAsCStringLen)
#else
import qualified Data.ByteString.Base   as B (unsafeUseAsCStringLen)
#endif
import qualified Data.ByteString      as B
import Foreign
import Foreign.C.Types
import Numeric                        (showHex)
import System.IO.Unsafe

md5_digest_length :: Int
md5_digest_length = 16

--
-- | Fast md5 using OpenSSL. The md5 hash should be referentially transparent..
-- The ByteString is guaranteed not to be copied.
--
-- The result string should be identical to the output of MD5(1).
-- That is:
--
-- > $ md5 /usr/share/dict/words 
-- > MD5 (/usr/share/dict/words) = e5c152147e93b81424c13772330e74b3
--
-- While this md5sum binding will return:
--
md5sum :: B.ByteString -> String
md5sum p = unsafePerformIO $ B.unsafeUseAsCStringLen p $ \(ptr,n) -> do
    allocaBytes md5_digest_length $ \digest_ptr -> do
        digest  <- c_md5 ptr (fromIntegral n) digest_ptr
        go digest 0 []
  where

    -- print it in 0-padded hex format
    go :: Ptr Word8 -> Int -> [String] -> IO String
#ifndef __HADDOCK__
    go q n acc
        | n `seq` q `seq` False = undefined
        | n >= 16   = return $ concat (reverse acc)
        | otherwise = do w <- peekElemOff q n
                         go q (n+1) (draw w : acc)

    draw w = case showHex w [] of
                [x] -> ['0', x]
                x   -> x
#endif

--
-- unsigned char *MD5(const unsigned char *d, unsigned long n, unsigned char *md);
--
foreign import ccall "openssl/md5.h MD5" c_md5
    :: Ptr CChar -> CULong -> Ptr CChar -> IO (Ptr Word8)
