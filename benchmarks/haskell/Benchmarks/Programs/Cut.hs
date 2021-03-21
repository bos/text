-- | Cut into a file, selecting certain columns (e.g. columns 10 to 40)
--
-- Tested in this benchmark:
--
-- * Reading the file
--
-- * Splitting into lines
--
-- * Taking a number of characters from the lines
--
-- * Joining the lines
--
-- * Writing back to a handle
--
module Benchmarks.Programs.Cut
    ( benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnfIO)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL

benchmark :: FilePath -> Handle -> Int -> Int -> Benchmark
benchmark p sink from to = bgroup "Cut"
    [ bench' "Text" text
    , bench' "LazyText" lazyText
    , bench' "TextByteString" textByteString
    , bench' "LazyTextByteString" lazyTextByteString
    ]
  where
    bench' n s = bench n $ whnfIO (s p sink from to)

text :: FilePath -> Handle -> Int -> Int -> IO ()
text fp sink from to = do
    t <- T.readFile fp
    T.hPutStr sink $ cut t
  where
    cut = T.unlines . map (T.take (to - from) . T.drop from) . T.lines

lazyText :: FilePath -> Handle -> Int -> Int -> IO ()
lazyText fp sink from to = do
    t <- TL.readFile fp
    TL.hPutStr sink $ cut t
  where
    cut = TL.unlines . map (TL.take (to' - from') . TL.drop from') . TL.lines
    from' = fromIntegral from
    to' = fromIntegral to

textByteString :: FilePath -> Handle -> Int -> Int -> IO ()
textByteString fp sink from to = do
    t <- T.decodeUtf8 `fmap` B.readFile fp
    B.hPutStr sink $ T.encodeUtf8 $ cut t
  where
    cut = T.unlines . map (T.take (to - from) . T.drop from) . T.lines

lazyTextByteString :: FilePath -> Handle -> Int -> Int -> IO ()
lazyTextByteString fp sink from to = do
    t <- TL.decodeUtf8 `fmap` BL.readFile fp
    BL.hPutStr sink $ TL.encodeUtf8 $ cut t
  where
    cut = TL.unlines . map (TL.take (to' - from') . TL.drop from') . TL.lines
    from' = fromIntegral from
    to' = fromIntegral to
