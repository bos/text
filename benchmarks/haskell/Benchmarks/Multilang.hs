{-# LANGUAGE BangPatterns, OverloadedStrings, RankNTypes #-}

module Benchmarks.Multilang (benchmark) where

import qualified Data.ByteString as B
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import Test.Tasty.Bench (Benchmark, bgroup, bench, env, nf)

readYiwiki :: IO Text
readYiwiki = decodeUtf8 `fmap` B.readFile "benchmarks/text-test-data/yiwiki.xml"

benchmark :: Benchmark
benchmark = env readYiwiki $ \content -> bgroup "Multilang"
  [ bench "find_first" $ nf (Text.isInfixOf "en:Benin") content
  , bench "find_index" $ nf (Text.findIndex (=='c')) content
  ]
