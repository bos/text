{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,5,0)
-- base-4.5.0 is 7.4, default sigs introduced in 7.2
{-# LANGUAGE DefaultSignatures #-}
#endif
{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Unsafe.Shift
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fast, unchecked bit shifting functions.

module Data.Text.Internal.Unsafe.Shift
    (
      UnsafeShift(..)
    ) where

#if MIN_VERSION_base(4,5,0)
import qualified Data.Bits as Bits
import Data.Word
#else
import GHC.Base
import GHC.Word
#endif

-- | This is a workaround for poor optimisation in GHC 6.8.2.  It
-- fails to notice constant-width shifts, and adds a test and branch
-- to every shift.  This imposes about a 10% performance hit.
--
-- These functions are undefined when the amount being shifted by is
-- greater than the size in bits of a machine Int#.
class UnsafeShift a where
    shiftL :: a -> Int -> a
#if MIN_VERSION_base(4,5,0)
    {-# INLINABLE shiftL #-}
    default shiftL :: Bits.Bits a => a -> Int -> a
    shiftL = Bits.unsafeShiftL
#endif

    shiftR :: a -> Int -> a
#if MIN_VERSION_base(4,5,0)
    {-# INLINABLE shiftR #-}
    default shiftR :: Bits.Bits a => a -> Int -> a
    shiftR = Bits.unsafeShiftR
#endif

instance UnsafeShift Word16 where
#if !MIN_VERSION_base(4,5,0)
    {-# INLINABLE shiftL #-}
    shiftL (W16# x#) (I# i#) = W16# (narrow16Word# (x# `uncheckedShiftL#` i#))

    {-# INLINABLE shiftR #-}
    shiftR (W16# x#) (I# i#) = W16# (x# `uncheckedShiftRL#` i#)
#endif

instance UnsafeShift Word32 where
#if !MIN_VERSION_base(4,5,0)
    {-# INLINABLE shiftL #-}
    shiftL (W32# x#) (I# i#) = W32# (narrow32Word# (x# `uncheckedShiftL#` i#))

    {-# INLINABLE shiftR #-}
    shiftR (W32# x#) (I# i#) = W32# (x# `uncheckedShiftRL#` i#)
#endif

instance UnsafeShift Word64 where
#if !MIN_VERSION_base(4,5,0)
    {-# INLINABLE shiftL #-}
    shiftL (W64# x#) (I# i#) = W64# (x# `uncheckedShiftL64#` i#)

    {-# INLINABLE shiftR #-}
    shiftR (W64# x#) (I# i#) = W64# (x# `uncheckedShiftRL64#` i#)
#endif

instance UnsafeShift Int where
#if !MIN_VERSION_base(4,5,0)
    {-# INLINABLE shiftL #-}
    shiftL (I# x#) (I# i#) = I# (x# `iShiftL#` i#)

    {-# INLINABLE shiftR #-}
    shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)
#endif

{-
instance UnsafeShift Integer where
    {-# INLINABLE shiftL #-}
    shiftL = Bits.shiftL

    {-# INLINABLE shiftR #-}
    shiftR = Bits.shiftR
-}
