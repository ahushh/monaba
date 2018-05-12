module Main where

import Test.QuickCheck
import System.IO.Unsafe
import System.IO
import System.Process
import Data.ByteString (pack)
import Data.ByteString.Char8 (unpack)
import Data.Char
import Data.Word
import System.Random
import System.Directory

import Data.Digest.OpenSSL.MD5
import Run

main = quickCheck prop_md5sum

prop_md5sum :: [Word8] -> Bool
prop_md5sum x = md5sum (pack x) == model_md5 (toString x)

toString :: [Word8] -> String
toString = unpack . pack

-- [] --> "d41d8cd98f00b204e9800998ecf8427e"

model_md5 :: String -> String
model_md5 x = unsafePerformIO $ do
        f <- do writeFile "test.txt" x
                return "test.txt"

        v <- readProcess "md5" [f] []
        removeFile f
        return $! case v of
            Left e   -> "ERROR: " ++ show e
            Right s  -> reverse. tail. takeWhile (/= ' ') . reverse $ s

            -- "MD5 (test.txt) = d41d8cd98f00b204e9800998ecf8427e\n"

{- 
*Main> quickCheck prop_md5sum 
OK, passed 100 tests.
-}


------------------------------------------------------------------------

instance Arbitrary Word8 where
    arbitrary     = choose (minBound, maxBound)
    coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 4))

instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x, g)


------------------------------------------------------------------------
