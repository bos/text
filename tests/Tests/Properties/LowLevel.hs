-- | Test low-level operations

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Tests.Properties.LowLevel (testLowLevel) where

import Control.Applicative ((<$>), pure)
import Control.Exception as E (SomeException, catch, evaluate)
import Data.Int (Int32, Int64)
import Data.Text.Foreign
import Data.Text.Internal (mul, mul32, mul64)
import Data.Word (Word16, Word32)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck hiding ((.&.))
import Tests.QuickCheckUtils
import Tests.Utils
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified System.IO as IO

mulRef :: (Integral a, Bounded a) => a -> a -> Maybe a
mulRef a b
  | ab < bot || ab > top = Nothing
  | otherwise            = Just (fromIntegral ab)
  where ab  = fromIntegral a * fromIntegral b
        top = fromIntegral (maxBound `asTypeOf` a) :: Integer
        bot = fromIntegral (minBound `asTypeOf` a) :: Integer

eval :: (a -> b -> c) -> a -> b -> Maybe c
eval f a b = unsafePerformIO $
  (Just <$> evaluate (f a b)) `E.catch` (\(_::SomeException) -> pure Nothing)

t_mul32 :: Int32 -> Int32 -> Property
t_mul32 a b = mulRef a b === eval mul32 a b

t_mul64 :: Int64 -> Int64 -> Property
t_mul64 a b = mulRef a b === eval mul64 a b

t_mul :: Int -> Int -> Property
t_mul a b = mulRef a b === eval mul a b

-- Misc.

t_dropWord16 m t = dropWord16 m t `T.isSuffixOf` t
t_takeWord16 m t = takeWord16 m t `T.isPrefixOf` t
t_take_drop_16 (Small n) t = T.append (takeWord16 n t) (dropWord16 n t) === t
t_use_from t = ioProperty $ (==t) <$> useAsPtr t fromPtr

t_copy t = T.copy t === t

-- Input and output.

-- t_put_get = write_read T.unlines T.filter put get
--   where put h = withRedirect h IO.stdout . T.putStr
--         get h = withRedirect h IO.stdin T.getContents
-- tl_put_get = write_read TL.unlines TL.filter put get
--   where put h = withRedirect h IO.stdout . TL.putStr
--         get h = withRedirect h IO.stdin TL.getContents
t_write_read = write_read T.unlines T.filter T.hPutStr T.hGetContents
tl_write_read = write_read TL.unlines TL.filter TL.hPutStr TL.hGetContents

t_write_read_line m b t = write_read head T.filter T.hPutStrLn
                            T.hGetLine m b [t]
tl_write_read_line m b t = write_read head TL.filter TL.hPutStrLn
                             TL.hGetLine m b [t]


testLowLevel :: TestTree
testLowLevel =
  testGroup "lowlevel" [
    testGroup "mul" [
      testProperty "t_mul" t_mul,
      testProperty "t_mul32" t_mul32,
      testProperty "t_mul64" t_mul64
    ],

    testGroup "misc" [
      testProperty "t_dropWord16" t_dropWord16,
      testProperty "t_takeWord16" t_takeWord16,
      testProperty "t_take_drop_16" t_take_drop_16,
      testProperty "t_use_from" t_use_from,
      testProperty "t_copy" t_copy
    ],

    testGroup "input-output" [
      testProperty "t_write_read" t_write_read,
      testProperty "tl_write_read" tl_write_read,
      testProperty "t_write_read_line" t_write_read_line,
      testProperty "tl_write_read_line" tl_write_read_line
      -- These tests are subject to I/O race conditions
      -- testProperty "t_put_get" t_put_get,
      -- testProperty "tl_put_get" tl_put_get
    ]
  ]

