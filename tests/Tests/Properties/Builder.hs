-- | Test @Builder@

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tests.Properties.Builder
    ( testBuilder
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word
import Numeric (showEFloat, showFFloat, showGFloat, showHex)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Tests.QuickCheckUtils
import qualified Data.List as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB

-- Builder.

tb_singleton   = id `eqP` (unpackS . TB.toLazyText . mconcat . map TB.singleton)
tb_fromString  = id `eq` (TL.unpack . TB.toLazyText . TB.fromString)
tb_fromText    = id `eqP` (unpackS . TB.toLazyText . TB.fromText)
tb_fromStrings = L.concat `eq` (TL.unpack . TB.toLazyText . mconcat . map TB.fromString)
tb_fromTexts   = L.concat `eq` (unpackS . TB.toLazyText . mconcat . map (TB.fromText . packS))

tb_associative s1 s2 s3 =
    TB.toLazyText (b1 `mappend` (b2 `mappend` b3)) ===
    TB.toLazyText ((b1 `mappend` b2) `mappend` b3)
  where b1 = TB.fromText (packS s1)
        b2 = TB.fromText (packS s2)
        b3 = TB.fromText (packS s3)

-- Numeric builder stuff.

tb_decimal :: (Integral a, Show a) => a -> Property
tb_decimal = (TB.toLazyText . TB.decimal) `eq` (TL.pack . show)

tb_decimal_integer (a::Integer) = tb_decimal a
tb_decimal_integer_big (Big a) = tb_decimal a
tb_decimal_int (a::Int) = tb_decimal a
tb_decimal_int8 (a::Int8) = tb_decimal a
tb_decimal_int16 (a::Int16) = tb_decimal a
tb_decimal_int32 (a::Int32) = tb_decimal a
tb_decimal_int64 (a::Int64) = tb_decimal a
tb_decimal_word (a::Word) = tb_decimal a
tb_decimal_word8 (a::Word8) = tb_decimal a
tb_decimal_word16 (a::Word16) = tb_decimal a
tb_decimal_word32 (a::Word32) = tb_decimal a
tb_decimal_word64 (a::Word64) = tb_decimal a

tb_decimal_big_int (Large (a::Int)) = tb_decimal a
tb_decimal_big_int64 (Large (a::Int64)) = tb_decimal a
tb_decimal_big_word (Large (a::Word)) = tb_decimal a
tb_decimal_big_word64 (Large (a::Word64)) = tb_decimal a

tb_hex :: (Integral a, Show a) => a -> Property
tb_hex = (TB.toLazyText . TB.hexadecimal) `eq` (TL.pack . flip showHex "")

tb_hexadecimal_integer (a::Integer) = tb_hex a
tb_hexadecimal_int (a::Int) = tb_hex a
tb_hexadecimal_int8 (a::Int8) = tb_hex a
tb_hexadecimal_int16 (a::Int16) = tb_hex a
tb_hexadecimal_int32 (a::Int32) = tb_hex a
tb_hexadecimal_int64 (a::Int64) = tb_hex a
tb_hexadecimal_word (a::Word) = tb_hex a
tb_hexadecimal_word8 (a::Word8) = tb_hex a
tb_hexadecimal_word16 (a::Word16) = tb_hex a
tb_hexadecimal_word32 (a::Word32) = tb_hex a
tb_hexadecimal_word64 (a::Word64) = tb_hex a

tb_realfloat :: (RealFloat a, Show a) => a -> Property
tb_realfloat = (TB.toLazyText . TB.realFloat) `eq` (TL.pack . show)

tb_realfloat_float (a::Float) = tb_realfloat a
tb_realfloat_double (a::Double) = tb_realfloat a

showFloat :: (RealFloat a) => TB.FPFormat -> Maybe Int -> a -> ShowS
showFloat TB.Exponent (Just 0) = showEFloat (Just 1) -- see gh-231
showFloat TB.Exponent p = showEFloat p
showFloat TB.Fixed    p = showFFloat p
showFloat TB.Generic  p = showGFloat p

tb_formatRealFloat :: (RealFloat a, Show a) =>
                      a -> TB.FPFormat -> Precision a -> Property
tb_formatRealFloat a fmt prec = cond ==>
    TB.formatRealFloat fmt p a ===
    TB.fromString (showFloat fmt p a "")
  where p = precision a prec
        cond = case (p,fmt) of
#if MIN_VERSION_base(4,12,0)
                  (Just 0, TB.Generic) -> False -- skipping due to gh-231
#endif
                  _                    -> True

tb_formatRealFloat_float (a::Float) = tb_formatRealFloat a
tb_formatRealFloat_double (a::Double) = tb_formatRealFloat a

testBuilder :: TestTree
testBuilder =
  testGroup "builder" [
    testProperty "tb_singleton" tb_singleton,
    testProperty "tb_fromString" tb_fromString,
    testProperty "tb_fromText" tb_fromText,
    testProperty "tb_fromStrings" tb_fromStrings,
    testProperty "tb_fromTexts" tb_fromTexts,
    testProperty "tb_associative" tb_associative,
    testGroup "decimal" [
      testProperty "tb_decimal_int" tb_decimal_int,
      testProperty "tb_decimal_int8" tb_decimal_int8,
      testProperty "tb_decimal_int16" tb_decimal_int16,
      testProperty "tb_decimal_int32" tb_decimal_int32,
      testProperty "tb_decimal_int64" tb_decimal_int64,
      testProperty "tb_decimal_integer" tb_decimal_integer,
      testProperty "tb_decimal_integer_big" tb_decimal_integer_big,
      testProperty "tb_decimal_word" tb_decimal_word,
      testProperty "tb_decimal_word8" tb_decimal_word8,
      testProperty "tb_decimal_word16" tb_decimal_word16,
      testProperty "tb_decimal_word32" tb_decimal_word32,
      testProperty "tb_decimal_word64" tb_decimal_word64,
      testProperty "tb_decimal_big_int" tb_decimal_big_int,
      testProperty "tb_decimal_big_word" tb_decimal_big_word,
      testProperty "tb_decimal_big_int64" tb_decimal_big_int64,
      testProperty "tb_decimal_big_word64" tb_decimal_big_word64
    ],
    testGroup "hexadecimal" [
      testProperty "tb_hexadecimal_int" tb_hexadecimal_int,
      testProperty "tb_hexadecimal_int8" tb_hexadecimal_int8,
      testProperty "tb_hexadecimal_int16" tb_hexadecimal_int16,
      testProperty "tb_hexadecimal_int32" tb_hexadecimal_int32,
      testProperty "tb_hexadecimal_int64" tb_hexadecimal_int64,
      testProperty "tb_hexadecimal_integer" tb_hexadecimal_integer,
      testProperty "tb_hexadecimal_word" tb_hexadecimal_word,
      testProperty "tb_hexadecimal_word8" tb_hexadecimal_word8,
      testProperty "tb_hexadecimal_word16" tb_hexadecimal_word16,
      testProperty "tb_hexadecimal_word32" tb_hexadecimal_word32,
      testProperty "tb_hexadecimal_word64" tb_hexadecimal_word64
    ],
    testGroup "realfloat" [
      testProperty "tb_realfloat_double" tb_realfloat_double,
      testProperty "tb_realfloat_float" tb_realfloat_float,
      testProperty "tb_formatRealFloat_float" tb_formatRealFloat_float,
      testProperty "tb_formatRealFloat_double" tb_formatRealFloat_double
    ]
  ]
