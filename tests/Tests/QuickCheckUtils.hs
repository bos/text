-- | This module provides quickcheck utilities, e.g. arbitrary and show
-- instances, and comparison functions, so we can focus on the actual properties
-- in the 'Tests.Properties' module.
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.QuickCheckUtils
    ( BigInt(..)
    , NotEmpty(..)
    , Sqrt(..)
    , SpacyString(..)

    , Precision(..)
    , precision

    , DecodeErr(..)
    , genDecodeErr

    , Stringy(..)
    , unpack2
    , eq
    , eqP
    , eqPSqrt

    , write_read
    ) where

import Control.Arrow ((***))
import Control.DeepSeq (NFData (..), deepseq)
import Control.Exception (bracket)
import Data.Char (isSpace)
import Data.Text.Foreign (I16)
import Data.Text.Lazy.Builder.RealFloat (FPFormat(..))
import Data.Word (Word8, Word16)
import Test.QuickCheck hiding (Fixed(..), Small (..), (.&.))
import Tests.Utils
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Internal.Fusion as TF
import qualified Data.Text.Internal.Fusion.Common as TF
import qualified Data.Text.Internal.Lazy as TL
import qualified Data.Text.Internal.Lazy.Fusion as TLF
import qualified Data.Text.Lazy as TL
import qualified System.IO as IO

genWord8 :: Gen Word8
genWord8 = chooseAny

instance Arbitrary I16 where
    arbitrary     = arbitrarySizedIntegral
    shrink        = shrinkIntegral

instance Arbitrary B.ByteString where
    arbitrary     = B.pack `fmap` listOf genWord8
    shrink        = map B.pack . shrink . B.unpack

instance Arbitrary BL.ByteString where
    arbitrary = oneof
      [ BL.fromChunks <$> arbitrary
      -- so that a single utf8 code point could appear split over up to 4 chunks
      , BL.fromChunks . map B.singleton <$> listOf genWord8
      -- so that a code point with 4 byte long utf8 representation
      -- could appear split over 3 non-singleton chunks
      , (\a b c -> BL.fromChunks [a, b, c])
        <$> arbitrary
        <*> ((\a b -> B.pack [a, b]) <$> genWord8 <*> genWord8)
        <*> arbitrary
      ]
    shrink xs = BL.fromChunks <$> shrink (BL.toChunks xs)

-- | For tests that have O(n^2) running times or input sizes, resize
-- their inputs to the square root of the originals.
newtype Sqrt a = Sqrt { unSqrt :: a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Sqrt a) where
    arbitrary = fmap Sqrt $ sized $ \n -> resize (smallish n) arbitrary
        where
            smallish = round . (sqrt :: Double -> Double) . fromIntegral . abs
    shrink = map Sqrt . shrink . unSqrt

instance Arbitrary T.Text where
    arbitrary = (T.pack . getUnicodeString) `fmap` arbitrary
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary TL.Text where
    arbitrary = (TL.fromChunks . map notEmpty . unSqrt) `fmap` arbitrary
    shrink = map TL.pack . shrink . TL.unpack

newtype BigInt = Big Integer
               deriving (Eq, Show)

instance Arbitrary BigInt where
    arbitrary = choose (1::Int,200) >>= \e -> Big <$> choose (10^(e-1),10^e)
    shrink (Big a) = [Big (a `div` 2^(l-e)) | e <- shrink l]
      where l = truncate (log (fromIntegral a) / log 2 :: Double) :: Integer

newtype NotEmpty a = NotEmpty { notEmpty :: a }
    deriving (Eq, Ord, Show)

instance Arbitrary (NotEmpty T.Text) where
    arbitrary   = fmap (NotEmpty . T.pack . getNonEmpty) arbitrary
    shrink      = fmap (NotEmpty . T.pack . getNonEmpty)
                . shrink . NonEmpty . T.unpack . notEmpty

instance Arbitrary (NotEmpty TL.Text) where
    arbitrary   = fmap (NotEmpty . TL.pack . getNonEmpty) arbitrary
    shrink      = fmap (NotEmpty . TL.pack . getNonEmpty)
                . shrink . NonEmpty . TL.unpack . notEmpty

data DecodeErr = Lenient | Ignore | Strict | Replace
               deriving (Show, Eq, Bounded, Enum)

genDecodeErr :: DecodeErr -> Gen T.OnDecodeError
genDecodeErr Lenient = return T.lenientDecode
genDecodeErr Ignore  = return T.ignore
genDecodeErr Strict  = return T.strictDecode
genDecodeErr Replace = (\c _ _ -> c) <$> frequency
  [ (1, return Nothing)
  , (50, Just <$> choose ('\x1', '\xffff'))
  ]

instance Arbitrary DecodeErr where
    arbitrary = arbitraryBoundedEnum

class Stringy s where
    packS    :: String -> s
    unpackS  :: s -> String
    splitAtS :: Int -> s -> (s,s)
    packSChunkSize :: Int -> String -> s
    packSChunkSize _ = packS

instance Stringy String where
    packS    = id
    unpackS  = id
    splitAtS = splitAt

instance Stringy (TF.Stream Char) where
    packS        = TF.streamList
    unpackS      = TF.unstreamList
    splitAtS n s = (TF.take n s, TF.drop n s)

instance Stringy T.Text where
    packS    = T.pack
    unpackS  = T.unpack
    splitAtS = T.splitAt

instance Stringy TL.Text where
    packSChunkSize k = TLF.unstreamChunks k . TF.streamList
    packS    = TL.pack
    unpackS  = TL.unpack
    splitAtS = ((TL.lazyInvariant *** TL.lazyInvariant) .) .
               TL.splitAt . fromIntegral

unpack2 :: (Stringy s) => (s,s) -> (String,String)
unpack2 = unpackS *** unpackS

-- Do two functions give the same answer?
eq :: (Eq a, Show a) => (t -> a) -> (t -> a) -> t -> Property
eq a b s  = a s =^= b s

-- What about with the RHS packed?
eqP :: (Eq a, Show a, Stringy s) =>
       (String -> a) -> (s -> a) -> String -> Word8 -> Property
eqP f g s w  = counterexample "orig" (f s =^= g t) .&&.
               counterexample "mini" (f s =^= g mini) .&&.
               counterexample "head" (f sa =^= g ta) .&&.
               counterexample "tail" (f sb =^= g tb)
    where t             = packS s
          mini          = packSChunkSize 10 s
          (sa,sb)       = splitAt m s
          (ta,tb)       = splitAtS m t
          l             = length s
          m | l == 0    = n
            | otherwise = n `mod` l
          n             = fromIntegral w

eqPSqrt :: (Eq a, Show a, Stringy s) =>
       (String -> a) -> (s -> a) -> Sqrt String -> Word8 -> Property
eqPSqrt f g s = eqP f g (unSqrt s)

instance Arbitrary FPFormat where
    arbitrary = arbitraryBoundedEnum

newtype Precision a = Precision (Maybe Int)
                    deriving (Eq, Show)

precision :: a -> Precision a -> Maybe Int
precision _ (Precision prec) = prec

arbitraryPrecision :: Int -> Gen (Precision a)
arbitraryPrecision maxDigits = Precision <$> do
  n <- choose (-1,maxDigits)
  return $ if n == -1
           then Nothing
           else Just n

instance Arbitrary (Precision Float) where
    arbitrary = arbitraryPrecision 11
    shrink    = map Precision . shrink . precision undefined

instance Arbitrary (Precision Double) where
    arbitrary = arbitraryPrecision 22
    shrink    = map Precision . shrink . precision undefined

instance Arbitrary IO.Newline where
    arbitrary = oneof [return IO.LF, return IO.CRLF]

instance Arbitrary IO.NewlineMode where
    arbitrary = IO.NewlineMode <$> arbitrary <*> arbitrary

instance Arbitrary IO.BufferMode where
    arbitrary = oneof [ return IO.NoBuffering,
                        return IO.LineBuffering,
                        return (IO.BlockBuffering Nothing),
                        (IO.BlockBuffering . Just . (+1) . fromIntegral) `fmap`
                        (arbitrary :: Gen Word16) ]

-- This test harness is complex!  What property are we checking?
--
-- Reading after writing a multi-line file should give the same
-- results as were written.
--
-- What do we vary while checking this property?
-- * The lines themselves, scrubbed to contain neither CR nor LF.  (By
--   working with a list of lines, we ensure that the data will
--   sometimes contain line endings.)
-- * Newline translation mode.
-- * Buffering.
write_read :: (NFData a, Eq a, Show a)
           => ([b] -> a)
           -> ((Char -> Bool) -> a -> b)
           -> (IO.Handle -> a -> IO ())
           -> (IO.Handle -> IO a)
           -> IO.NewlineMode
           -> IO.BufferMode
           -> [a]
           -> Property
write_read _ _ _ _ (IO.NewlineMode IO.LF IO.CRLF) _ _ = discard
write_read unline filt writer reader nl buf ts = ioProperty $
    (===t) <$> act
  where
    t = unline . map (filt (not . (`elem` "\r\n"))) $ ts

    act = withTempFile $ \path h -> do
            IO.hSetNewlineMode h nl
            IO.hSetBuffering h buf
            () <- writer h t
            IO.hClose h
            bracket (IO.openFile path IO.ReadMode) IO.hClose $ \h' -> do
              IO.hSetNewlineMode h' nl
              IO.hSetBuffering h' buf
              r <- reader h'
              r `deepseq` return r

-- Generate various Unicode space characters with high probability
arbitrarySpacyChar :: Gen Char
arbitrarySpacyChar = oneof
  [ arbitraryUnicodeChar
  , elements $ filter isSpace [minBound..maxBound]
  ]

newtype SpacyString = SpacyString { getSpacyString :: String }
  deriving (Eq, Ord, Show, Read)

instance Arbitrary SpacyString where
  arbitrary = SpacyString `fmap` listOf arbitrarySpacyChar
  shrink (SpacyString xs) = SpacyString `fmap` shrink xs
