-- | Test folds, scans, and unfolds

{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tests.Properties.Folds
    ( testFolds
    ) where

import Control.Arrow (second)
import Data.Word (Word8, Word16)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Small(..), (===), applyFun, applyFun2)
import Tests.QuickCheckUtils
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as S
import qualified Data.Text.Internal.Fusion.Common as S
import qualified Data.Text.Lazy as TL

-- Folds

sf_foldl (applyFun -> p) (applyFun2 -> f) z =
    (L.foldl f z . L.filter p) `eqP` (S.foldl f z . S.filter p)
    where _types  = f :: Char -> Char -> Char
t_foldl (applyFun2 -> f) z       = L.foldl f z  `eqP` (T.foldl f z)
    where _types  = f :: Char -> Char -> Char
tl_foldl (applyFun2 -> f) z      = L.foldl f z  `eqP` (TL.foldl f z)
    where _types  = f :: Char -> Char -> Char
sf_foldl' (applyFun -> p) (applyFun2 -> f) z =
    (L.foldl' f z . L.filter p) `eqP` (S.foldl' f z . S.filter p)
    where _types  = f :: Char -> Char -> Char
t_foldl' (applyFun2 -> f) z      = L.foldl' f z `eqP` T.foldl' f z
    where _types  = f :: Char -> Char -> Char
tl_foldl' (applyFun2 -> f) z     = L.foldl' f z `eqP` TL.foldl' f z
    where _types  = f :: Char -> Char -> Char
sf_foldl1 (applyFun -> p) (applyFun2 -> f) =
    (L.foldl1 f . L.filter p) `eqP` (S.foldl1 f . S.filter p)
t_foldl1 (applyFun2 -> f)        = L.foldl1 f   `eqP` T.foldl1 f
tl_foldl1 (applyFun2 -> f)       = L.foldl1 f   `eqP` TL.foldl1 f
sf_foldl1' (applyFun -> p) (applyFun2 -> f) =
    (L.foldl1' f . L.filter p) `eqP` (S.foldl1' f . S.filter p)
t_foldl1' (applyFun2 -> f)       = L.foldl1' f  `eqP` T.foldl1' f
tl_foldl1' (applyFun2 -> f)      = L.foldl1' f  `eqP` TL.foldl1' f
sf_foldr (applyFun -> p) (applyFun2 -> f) z =
    (L.foldr f z . L.filter p) `eqP` (S.foldr f z . S.filter p)
    where _types  = f :: Char -> Char -> Char
t_foldr (applyFun2 -> f) z       = L.foldr f z  `eqP` T.foldr f z
    where _types  = f :: Char -> Char -> Char
tl_foldr (applyFun2 -> f) z      = L.foldr f z  `eqPSqrt` TL.foldr f z
    where _types  = f :: Char -> Char -> Char
sf_foldr1 (applyFun -> p) (applyFun2 -> f) =
    (L.foldr1 f . L.filter p) `eqPSqrt` (S.foldr1 f . S.filter p)
t_foldr1 (applyFun2 -> f)        = L.foldr1 f   `eqP` T.foldr1 f
tl_foldr1 (applyFun2 -> f)       = L.foldr1 f   `eqPSqrt` TL.foldr1 f

-- Special folds

s_concat_s        = (L.concat . unSqrt) `eq` (unpackS . S.unstream . S.concat . map packS . unSqrt)
sf_concat (applyFun -> p)
                  = (L.concat . map (L.filter p) . unSqrt) `eq`
                    (unpackS . S.concat . map (S.filter p . packS) . unSqrt)
t_concat          = (L.concat . unSqrt) `eq` (unpackS . T.concat . map packS . unSqrt)
tl_concat         = (L.concat . unSqrt) `eq` (unpackS . TL.concat . map TL.pack . unSqrt)
sf_concatMap (applyFun -> p) (applyFun -> f) =
    (L.concatMap f . L.filter p) `eqPSqrt` (unpackS . S.concatMap (packS . f) . S.filter p)
t_concatMap (applyFun -> f)
                  = L.concatMap f `eqPSqrt` (unpackS . T.concatMap (packS . f))
tl_concatMap (applyFun -> f)
                  = L.concatMap f `eqPSqrt` (unpackS . TL.concatMap (TL.pack . f))
sf_any (applyFun -> q) (applyFun -> p)
                  = (L.any p . L.filter q) `eqP` (S.any p . S.filter q)
t_any (applyFun -> p)
                  = L.any p       `eqP` T.any p
tl_any (applyFun -> p)
                  = L.any p       `eqP` TL.any p
sf_all (applyFun -> q) (applyFun -> p)
                  = (L.all p . L.filter q) `eqP` (S.all p . S.filter q)
t_all (applyFun -> p)
                  = L.all p       `eqP` T.all p
tl_all (applyFun -> p)
                  = L.all p       `eqP` TL.all p
sf_maximum (applyFun -> p)
                  = (L.maximum . L.filter p) `eqP` (S.maximum . S.filter p)
t_maximum         = L.maximum     `eqP` T.maximum
tl_maximum        = L.maximum     `eqP` TL.maximum
sf_minimum (applyFun -> p)
                  = (L.minimum . L.filter p) `eqP` (S.minimum . S.filter p)
t_minimum         = L.minimum     `eqP` T.minimum
tl_minimum        = L.minimum     `eqP` TL.minimum

-- Scans

sf_scanl (applyFun -> p) (applyFun2 -> f) z =
    (L.scanl f z . L.filter p) `eqP` (unpackS . S.scanl f z . S.filter p)
t_scanl (applyFun2 -> f) z       = L.scanl f z   `eqP` (unpackS . T.scanl f z)
tl_scanl (applyFun2 -> f) z      = L.scanl f z   `eqP` (unpackS . TL.scanl f z)
t_scanl1 (applyFun2 -> f)        = L.scanl1 f    `eqP` (unpackS . T.scanl1 f)
tl_scanl1 (applyFun2 -> f)       = L.scanl1 f    `eqP` (unpackS . TL.scanl1 f)
t_scanr (applyFun2 -> f) z       = L.scanr f z   `eqP` (unpackS . T.scanr f z)
tl_scanr (applyFun2 -> f) z      = L.scanr f z   `eqP` (unpackS . TL.scanr f z)
t_scanr1 (applyFun2 -> f)        = L.scanr1 f    `eqP` (unpackS . T.scanr1 f)
tl_scanr1 (applyFun2 -> f)       = L.scanr1 f    `eqP` (unpackS . TL.scanr1 f)

t_mapAccumL_char c t =
    snd (T.mapAccumL (const (const (0 :: Int, c))) 0 t) === T.replicate (T.length t) (T.singleton c)
t_mapAccumL (applyFun2 -> f) z   = L.mapAccumL f z `eqP` (second unpackS . T.mapAccumL f z)
    where _types  = f :: Int -> Char -> (Int,Char)
tl_mapAccumL_char c t =
    snd (TL.mapAccumL (const (const (0 :: Int, c))) 0 t) === TL.replicate (TL.length t) (TL.singleton c)
tl_mapAccumL (applyFun2 -> f) z  = L.mapAccumL f z `eqP` (second unpackS . TL.mapAccumL f z)
    where _types  = f :: Int -> Char -> (Int,Char)
t_mapAccumR_char c t =
    snd (T.mapAccumR (const (const (0 :: Int, c))) 0 t) === T.replicate (T.length t) (T.singleton c)
t_mapAccumR (applyFun2 -> f) z   = L.mapAccumR f z `eqP` (second unpackS . T.mapAccumR f z)
    where _types  = f :: Int -> Char -> (Int,Char)
tl_mapAccumR_char c t =
    snd (TL.mapAccumR (const (const (0 :: Int, c))) 0 t) === TL.replicate (TL.length t) (TL.singleton c)
tl_mapAccumR (applyFun2 -> f) z  = L.mapAccumR f z `eqP` (second unpackS . TL.mapAccumR f z)
    where _types  = f :: Int -> Char -> (Int,Char)

-- Unfolds

tl_repeat (Small n) = L.replicate n `eq` (unpackS . TL.take (fromIntegral n) . TL.repeat)

s_replicate (Small n) = (L.concat . L.replicate n) `eq` (unpackS . S.replicateI (fromIntegral n) . packS)

t_replicate_char (Small n) c =
    L.replicate n c === T.unpack (T.replicate n (T.singleton c))
tl_replicate_char (Small n) c =
    L.replicate n c === TL.unpack (TL.replicate (fromIntegral n) (TL.singleton c))
t_length_replicate_char (Small n) c =
    L.length (L.replicate n c) === T.length (T.replicate n (T.singleton c))
tl_length_replicate_char (Small n) c =
    L.genericLength (L.replicate n c) === TL.length (TL.replicate (fromIntegral n) (TL.singleton c))

t_replicate (Small n) =
    (L.concat . L.replicate n) `eqPSqrt` (unpackS . T.replicate n)
tl_replicate (Small n) =
    (L.concat . L.replicate n) `eqPSqrt` (unpackS . TL.replicate (fromIntegral n))
t_length_replicate (Small n) =
    (L.length . L.concat . L.replicate n) `eqPSqrt` (T.length . T.replicate n)
tl_length_replicate (Small n) =
    (L.genericLength . L.concat . L.replicate n) `eqPSqrt` (TL.length . TL.replicate (fromIntegral n))

tl_cycle n        = (L.take m . L.cycle) `eq`
                    (unpackS . TL.take (fromIntegral m) . TL.cycle . packS)
    where m = fromIntegral (n :: Word8)

tl_iterate (applyFun -> f) n
                  = (L.take m . L.iterate f) `eq`
                    (unpackS . TL.take (fromIntegral m) . TL.iterate f)
    where m = fromIntegral (n :: Word8)

unf :: Int -> Char -> Maybe (Char, Char)
unf n c | fromEnum c * 100 > n = Nothing
        | otherwise            = Just (c, succ c)

t_unfoldr n       = L.unfoldr (unf m) `eq` (unpackS . T.unfoldr (unf m))
    where m = fromIntegral (n :: Word16)
tl_unfoldr n      = L.unfoldr (unf m) `eq` (unpackS . TL.unfoldr (unf m))
    where m = fromIntegral (n :: Word16)
t_unfoldrN n m    = (L.take i . L.unfoldr (unf j)) `eq`
                         (unpackS . T.unfoldrN i (unf j))
    where i = fromIntegral (n :: Word16)
          j = fromIntegral (m :: Word16)
tl_unfoldrN n m   = (L.take i . L.unfoldr (unf j)) `eq`
                         (unpackS . TL.unfoldrN (fromIntegral i) (unf j))
    where i = fromIntegral (n :: Word16)
          j = fromIntegral (m :: Word16)

testFolds :: TestTree
testFolds =
  testGroup "folds-unfolds" [
    testGroup "folds" [
      testProperty "sf_foldl" sf_foldl,
      testProperty "t_foldl" t_foldl,
      testProperty "tl_foldl" tl_foldl,
      testProperty "sf_foldl'" sf_foldl',
      testProperty "t_foldl'" t_foldl',
      testProperty "tl_foldl'" tl_foldl',
      testProperty "sf_foldl1" sf_foldl1,
      testProperty "t_foldl1" t_foldl1,
      testProperty "tl_foldl1" tl_foldl1,
      testProperty "t_foldl1'" t_foldl1',
      testProperty "sf_foldl1'" sf_foldl1',
      testProperty "tl_foldl1'" tl_foldl1',
      testProperty "sf_foldr" sf_foldr,
      testProperty "t_foldr" t_foldr,
      testProperty "tl_foldr" tl_foldr,
      testProperty "sf_foldr1" sf_foldr1,
      testProperty "t_foldr1" t_foldr1,
      testProperty "tl_foldr1" tl_foldr1,

      testGroup "special" [
        testProperty "s_concat_s" s_concat_s,
        testProperty "sf_concat" sf_concat,
        testProperty "t_concat" t_concat,
        testProperty "tl_concat" tl_concat,
        testProperty "sf_concatMap" sf_concatMap,
        testProperty "t_concatMap" t_concatMap,
        testProperty "tl_concatMap" tl_concatMap,
        testProperty "sf_any" sf_any,
        testProperty "t_any" t_any,
        testProperty "tl_any" tl_any,
        testProperty "sf_all" sf_all,
        testProperty "t_all" t_all,
        testProperty "tl_all" tl_all,
        testProperty "sf_maximum" sf_maximum,
        testProperty "t_maximum" t_maximum,
        testProperty "tl_maximum" tl_maximum,
        testProperty "sf_minimum" sf_minimum,
        testProperty "t_minimum" t_minimum,
        testProperty "tl_minimum" tl_minimum
      ]
    ],

    testGroup "scans" [
      testProperty "sf_scanl" sf_scanl,
      testProperty "t_scanl" t_scanl,
      testProperty "tl_scanl" tl_scanl,
      testProperty "t_scanl1" t_scanl1,
      testProperty "tl_scanl1" tl_scanl1,
      testProperty "t_scanr" t_scanr,
      testProperty "tl_scanr" tl_scanr,
      testProperty "t_scanr1" t_scanr1,
      testProperty "tl_scanr1" tl_scanr1
    ],

    testGroup "mapAccum" [
      testProperty "t_mapAccumL_char" t_mapAccumL_char,
      testProperty "t_mapAccumL" t_mapAccumL,
      testProperty "tl_mapAccumL_char" tl_mapAccumL_char,
      testProperty "tl_mapAccumL" tl_mapAccumL,
      testProperty "t_mapAccumR_char" t_mapAccumR_char,
      testProperty "t_mapAccumR" t_mapAccumR,
      testProperty "tl_mapAccumR_char" tl_mapAccumR_char,
      testProperty "tl_mapAccumR" tl_mapAccumR
    ],

    testGroup "unfolds" [
      testProperty "tl_cycle" tl_cycle,
      testProperty "tl_iterate" tl_iterate,
      testProperty "t_unfoldr" t_unfoldr,
      testProperty "tl_unfoldr" tl_unfoldr,
      testProperty "t_unfoldrN" t_unfoldrN,
      testProperty "tl_unfoldrN" tl_unfoldrN
    ],

    testGroup "replicate" [
      testProperty "tl_repeat" tl_repeat,
      testProperty "s_replicate" s_replicate,
      testProperty "t_replicate_char" t_replicate_char,
      testProperty "tl_replicate_char" tl_replicate_char,
      testProperty "t_length_replicate_char" t_length_replicate_char,
      testProperty "tl_length_replicate_char" tl_length_replicate_char,
      testProperty "t_replicate" t_replicate,
      testProperty "tl_replicate" tl_replicate,
      testProperty "t_length_replicate" t_length_replicate,
      testProperty "tl_length_replicate" tl_length_replicate
    ]

  ]
