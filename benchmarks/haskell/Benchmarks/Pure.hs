-- | Benchmarks various pure functions from the Text library
--
-- Tested in this benchmark:
--
-- * Most pure functions defined the string types
--
{-# LANGUAGE BangPatterns, CPP, GADTs, MagicHash #-}
{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Benchmarks.Pure
    ( initEnv
    , benchmark
    ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Test.Tasty.Bench (Benchmark, bgroup, bench, nf)
import Data.Monoid (mappend, mempty)
import GHC.Base (Char (..), Int (..), chr#, ord#, (+#))
import GHC.Generics (Generic)
import GHC.Int (Int64)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as TL

data Env = Env
    { bsa :: !BS.ByteString
    , ta :: !T.Text
    , tb :: !T.Text
    , tla :: !TL.Text
    , tlb :: !TL.Text
    , bla :: !BL.ByteString
    , bsa_len :: !Int
    , ta_len :: !Int
    , bla_len :: !Int64
    , tla_len :: !Int64
    , tl :: [T.Text]
    , tll :: [TL.Text]
    } deriving (Generic)

instance NFData Env

initEnv :: FilePath -> IO Env
initEnv fp = do
    -- Evaluate stuff before actually running the benchmark, we don't want to
    -- count it here.

    -- ByteString A
    bsa     <- BS.readFile fp

    -- Text A/B, LazyText A/B
    ta      <- evaluate $ T.decodeUtf8 bsa
    tb      <- evaluate $ T.toUpper ta
    tla     <- evaluate $ TL.fromChunks (T.chunksOf 16376 ta)
    tlb     <- evaluate $ TL.fromChunks (T.chunksOf 16376 tb)

    bla     <- evaluate $ BL.fromChunks (chunksOf 16376 bsa)

    -- Lengths
    bsa_len <- evaluate $ BS.length bsa
    ta_len  <- evaluate $ T.length ta
    bla_len <- evaluate $ BL.length bla
    tla_len <- evaluate $ TL.length tla

    -- Lines
    tl      <- evaluate $ T.lines ta
    tll     <- evaluate $ TL.lines tla

    return Env{..}

benchmark :: String -> Env -> Benchmark
benchmark kind ~Env{..} =
    bgroup "Pure"
        [ bgroup "append"
            [ benchT   $ nf (T.append tb) ta
            , benchTL  $ nf (TL.append tlb) tla
            ]
        , bgroup "concat"
            [ benchT   $ nf T.concat tl
            , benchTL  $ nf TL.concat tll
            ]
        , bgroup "cons"
            [ benchT   $ nf (T.cons c) ta
            , benchTL  $ nf (TL.cons c) tla
            ]
        -- concatMap exceeds 4G heap size on current test data
        -- , bgroup "concatMap"
        --     [ benchT   $ nf (T.concatMap (T.replicate 3 . T.singleton)) ta
        --     , benchTL  $ nf (TL.concatMap (TL.replicate 3 . TL.singleton)) tla
        --     ]
        , bgroup "decode"
            [ benchT   $ nf T.decodeUtf8 bsa
            , benchTL  $ nf TL.decodeUtf8 bla
            ]
        , bgroup "decode'"
            [ benchT   $ nf T.decodeUtf8' bsa
            , benchTL  $ nf TL.decodeUtf8' bla
            ]
        , bgroup "drop"
            [ benchT   $ nf (T.drop (ta_len `div` 3)) ta
            , benchTL  $ nf (TL.drop (tla_len `div` 3)) tla
            ]
        , bgroup "encode"
            [ benchT   $ nf T.encodeUtf8 ta
            , benchTL  $ nf TL.encodeUtf8 tla
            ]
        , bgroup "filter"
            [ benchT   $ nf (T.filter p0) ta
            , benchTL  $ nf (TL.filter p0) tla
            ]
        , bgroup "filter.filter"
            [ benchT   $ nf (T.filter p1 . T.filter p0) ta
            , benchTL  $ nf (TL.filter p1 . TL.filter p0) tla
            ]
        , bgroup "foldl'"
            [ benchT   $ nf (T.foldl' len 0) ta
            , benchTL  $ nf (TL.foldl' len 0) tla
            ]
        , bgroup "foldr"
            [ benchT   $ nf (L.length . T.foldr (:) []) ta
            , benchTL  $ nf (L.length . TL.foldr (:) []) tla
            ]
        , bgroup "head"
            [ benchT   $ nf T.head ta
            , benchTL  $ nf TL.head tla
            ]
        , bgroup "init"
            [ benchT   $ nf T.init ta
            , benchTL  $ nf TL.init tla
            ]
        , bgroup "intercalate"
            [ benchT   $ nf (T.intercalate tsw) tl
            , benchTL  $ nf (TL.intercalate tlw) tll
            ]
        , bgroup "intersperse"
            [ benchT   $ nf (T.intersperse c) ta
            , benchTL  $ nf (TL.intersperse c) tla
            ]
        , bgroup "isInfixOf"
            [ benchT   $ nf (T.isInfixOf tsw) ta
            , benchTL  $ nf (TL.isInfixOf tlw) tla
            ]
        , bgroup "last"
            [ benchT   $ nf T.last ta
            , benchTL  $ nf TL.last tla
            ]
        , bgroup "map"
            [ benchT   $ nf (T.map f) ta
            , benchTL  $ nf (TL.map f) tla
            ]
        , bgroup "mapAccumL"
            [ benchT   $ nf (T.mapAccumL g 0) ta
            , benchTL  $ nf (TL.mapAccumL g 0) tla
            ]
        , bgroup "mapAccumR"
            [ benchT   $ nf (T.mapAccumR g 0) ta
            , benchTL  $ nf (TL.mapAccumR g 0) tla
            ]
        , bgroup "map.map"
            [ benchT   $ nf (T.map f . T.map f) ta
            , benchTL  $ nf (TL.map f . TL.map f) tla
            ]
        , bgroup "replicate char"
            [ benchT   $ nf (T.replicate bsa_len) (T.singleton c)
            , benchTL  $ nf (TL.replicate (fromIntegral bsa_len)) (TL.singleton c)
            ]
        , bgroup "replicate string"
            [ benchT   $ nf (T.replicate (bsa_len `div` T.length tsw)) tsw
            , benchTL  $ nf (TL.replicate (fromIntegral bsa_len `div` TL.length tlw)) tlw
            ]
        , bgroup "reverse"
            [ benchT   $ nf T.reverse ta
            , benchTL  $ nf TL.reverse tla
            ]
        , bgroup "take"
            [ benchT   $ nf (T.take (ta_len `div` 3)) ta
            , benchTL  $ nf (TL.take (tla_len `div` 3)) tla
            ]
        , bgroup "tail"
            [ benchT   $ nf T.tail ta
            , benchTL  $ nf TL.tail tla
            ]
        , bgroup "toLower"
            [ benchT   $ nf T.toLower ta
            , benchTL  $ nf TL.toLower tla
            ]
        , bgroup "toUpper"
            [ benchT   $ nf T.toUpper ta
            , benchTL  $ nf TL.toUpper tla
            ]
        , bgroup "uncons"
            [ benchT   $ nf T.uncons ta
            , benchTL  $ nf TL.uncons tla
            ]
        , bgroup "words"
            [ benchT   $ nf T.words ta
            , benchTL  $ nf TL.words tla
            ]
        , bgroup "zipWith"
            [ benchT   $ nf (T.zipWith min tb) ta
            , benchTL  $ nf (TL.zipWith min tlb) tla
            ]
        , bgroup "length"
            [ bgroup "cons"
                [ benchT   $ nf (T.length . T.cons c) ta
                , benchTL  $ nf (TL.length . TL.cons c) tla
                ]
            , bgroup "decode"
                [ benchT   $ nf (T.length . T.decodeUtf8) bsa
                , benchTL  $ nf (TL.length . TL.decodeUtf8) bla
                ]
            , bgroup "drop"
                [ benchT   $ nf (T.length . T.drop (ta_len `div` 3)) ta
                , benchTL  $ nf (TL.length . TL.drop (tla_len `div` 3)) tla
                ]
            , bgroup "filter"
                [ benchT   $ nf (T.length . T.filter p0) ta
                , benchTL  $ nf (TL.length . TL.filter p0) tla
                ]
            , bgroup "filter.filter"
                [ benchT   $ nf (T.length . T.filter p1 . T.filter p0) ta
                , benchTL  $ nf (TL.length . TL.filter p1 . TL.filter p0) tla
                ]
            , bgroup "init"
                [ benchT   $ nf (T.length . T.init) ta
                , benchTL  $ nf (TL.length . TL.init) tla
                ]
            , bgroup "intercalate"
                [ benchT   $ nf (T.length . T.intercalate tsw) tl
                , benchTL  $ nf (TL.length . TL.intercalate tlw) tll
                ]
            , bgroup "intersperse"
                [ benchT   $ nf (T.length . T.intersperse c) ta
                , benchTL  $ nf (TL.length . TL.intersperse c) tla
                ]
            , bgroup "map"
                [ benchT   $ nf (T.length . T.map f) ta
                , benchTL  $ nf (TL.length . TL.map f) tla
                ]
            , bgroup "map.map"
                [ benchT   $ nf (T.length . T.map f . T.map f) ta
                , benchTL  $ nf (TL.length . TL.map f . TL.map f) tla
                ]
            , bgroup "replicate char"
                [ benchT   $ nf (T.length . T.replicate bsa_len) (T.singleton c)
                , benchTL  $ nf (TL.length . TL.replicate (fromIntegral bsa_len)) (TL.singleton c)
                ]
            , bgroup "replicate string"
                [ benchT   $ nf (T.length . T.replicate (bsa_len `div` T.length tsw)) tsw
                , benchTL  $ nf (TL.length . TL.replicate (fromIntegral bsa_len `div` TL.length tlw)) tlw
                ]
            , bgroup "take"
                [ benchT   $ nf (T.length . T.take (ta_len `div` 3)) ta
                , benchTL  $ nf (TL.length . TL.take (tla_len `div` 3)) tla
                ]
            , bgroup "tail"
                [ benchT   $ nf (T.length . T.tail) ta
                , benchTL  $ nf (TL.length . TL.tail) tla
                ]
            , bgroup "toLower"
                [ benchT   $ nf (T.length . T.toLower) ta
                , benchTL  $ nf (TL.length . TL.toLower) tla
                ]
            , bgroup "toUpper"
                [ benchT   $ nf (T.length . T.toUpper) ta
                , benchTL  $ nf (TL.length . TL.toUpper) tla
                ]
            , bgroup "words"
                [ benchT   $ nf (L.length . T.words) ta
                , benchTL  $ nf (L.length . TL.words) tla
                ]
            , bgroup "zipWith"
                [ benchT   $ nf (T.length . T.zipWith min tb) ta
                , benchTL  $ nf (TL.length . TL.zipWith min tlb) tla
                ]
              ]
        , bgroup "Builder"
            [ bench "mappend char" $ nf (TL.length . TB.toLazyText . mappendNChar 'a') 10000
            , bench "mappend 8 char" $ nf (TL.length . TB.toLazyText . mappend8Char) 'a'
            , bench "mappend text" $ nf (TL.length . TB.toLazyText . mappendNText short) 10000
            ]
        ]
  where
    benchT   = bench ("Text+" ++ kind)
    benchTL  = bench ("LazyText+" ++ kind)

    c  = 'й'
    p0 = (== c)
    p1 = (/= 'д')
    lw  = "право"
    tsw  = T.pack lw
    tlw  = TL.fromChunks [tsw]
    f (C# c#) = C# (chr# (ord# c# +# 1#))
    g (I# i#) (C# c#) = (I# (i# +# 1#), C# (chr# (ord# c# +# i#)))
    len l _ = l + (1::Int)
    short = T.pack "short"

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData BS.ByteString

instance NFData BL.ByteString where
    rnf BL.Empty        = ()
    rnf (BL.Chunk _ ts) = rnf ts
#endif

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

-- | Split a bytestring in chunks
--
chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf k = go
  where
    go t = case BS.splitAt k t of
             (a,b) | BS.null a -> []
                   | otherwise -> a : go b

-- | Append a character n times
--
mappendNChar :: Char -> Int -> TB.Builder
mappendNChar c n = go 0
  where
    go i
      | i < n     = TB.singleton c `mappend` go (i+1)
      | otherwise = mempty

-- | Gives more opportunity for inlining and elimination of unnecesary
-- bounds checks.
--
mappend8Char :: Char -> TB.Builder
mappend8Char c = TB.singleton c `mappend` TB.singleton c `mappend`
                 TB.singleton c `mappend` TB.singleton c `mappend`
                 TB.singleton c `mappend` TB.singleton c `mappend`
                 TB.singleton c `mappend` TB.singleton c

-- | Append a text N times
--
mappendNText :: T.Text -> Int -> TB.Builder
mappendNText t n = go 0
  where
    go i
      | i < n     = TB.fromText t `mappend` go (i+1)
      | otherwise = mempty
