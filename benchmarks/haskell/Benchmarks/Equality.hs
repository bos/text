-- | Compare a string with a copy of itself that is identical except
-- for the last character.
--
-- Tested in this benchmark:
--
-- * Comparison of strings (Eq instance)
--
module Benchmarks.Equality
    ( initEnv
    , benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnf)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

type Env = (T.Text, TL.Text)

initEnv :: FilePath -> IO Env
initEnv fp = do
  b <- B.readFile fp
  bl1 <- BL.readFile fp
  return (T.decodeUtf8 b, TL.decodeUtf8 bl1)

benchmark :: Env -> Benchmark
benchmark ~(t, tl) =
  bgroup "Equality"
    [ bench "Text" $ whnf (== T.init t `T.snoc` '\xfffd') t
    , bench "LazyText" $ whnf (== TL.init tl `TL.snoc` '\xfffd') tl
    ]
