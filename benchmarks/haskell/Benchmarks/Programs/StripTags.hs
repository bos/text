-- | Program to replace HTML tags by whitespace
--
-- This program was originally contributed by Petr Prokhorenkov.
--
-- Tested in this benchmark:
--
-- * Reading the file
--
-- * Replacing text between HTML tags (<>) with whitespace
--
-- * Writing back to a handle
--
{-# OPTIONS_GHC -fspec-constr-count=5 #-}
module Benchmarks.Programs.StripTags
    ( benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnfIO)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

benchmark :: FilePath -> Handle -> Benchmark
benchmark i o = bgroup "StripTags"
    [ bench "Text" $ whnfIO $ T.readFile i >>= T.hPutStr o . text
    , bench "TextByteString" $ whnfIO $
        B.readFile i >>= B.hPutStr o . T.encodeUtf8 . text . T.decodeUtf8
    ]

text :: T.Text -> T.Text
text = snd . T.mapAccumL step 0

step :: Int -> Char -> (Int, Char)
step d c
    | d > 0 || d' > 0 = (d', ' ')
    | otherwise       = (d', c)
  where
    d' = d + depth c
    depth '>' = 1
    depth '<' = -1
    depth _   = 0
